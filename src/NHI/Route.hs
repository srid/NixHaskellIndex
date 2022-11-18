{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NHI.Route where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Sequence (chunksOf)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Ema
import Ema.Route.Generic
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Ema.Route.Lib.Extra.StringRoute (StringRoute (StringRoute))
import Ema.Route.Prism (Prism_)
import Generics.SOP qualified as SOP
import NHI.Types (NixData, Pkg (..))
import Optics.Core

data Model = Model
  { modelBaseUrl :: Text
  , modelStatic :: SR.Model
  , modelData :: NixData
  }
  deriving stock (Eq, Show, Generic)

newtype Page = Page {unPage :: Int}
  deriving newtype (Show, Eq, Ord)

instance IsString Page where
  fromString = Page . fromJust . readMaybe

instance ToString Page where
  toString = show . unPage

class Paged a where
  pageSize :: Proxy a -> Int
  pageSize Proxy = 500
  pages :: [a] -> [[a]]
  pages xs = fmap toList . toList $ chunksOf (pageSize @a Proxy) (Seq.fromList xs)

instance Paged (NonEmpty a)
instance Paged (Text, NonEmpty a)

data PaginatedRoute (t :: Type) = PaginatedRoute_Main | PaginatedRoute_OnPage Page
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

getPage :: forall {t}. PaginatedRoute t -> Page
getPage = \case
  PaginatedRoute_Main -> Page 1
  PaginatedRoute_OnPage p -> p

fromPage :: forall {t}. Page -> PaginatedRoute t
fromPage = \case
  Page 1 -> PaginatedRoute_Main
  p -> PaginatedRoute_OnPage p

instance IsRoute (PaginatedRoute a) where
  type RouteModel (PaginatedRoute a) = [[a]]
  routePrism m =
    toPrism_ $
      prism'
        ( \case
            PaginatedRoute_Main -> "index.html"
            PaginatedRoute_OnPage page -> "page/" <> toString page
        )
        ( \fp -> do
            if fp == "index.html"
              then pure PaginatedRoute_Main
              else do
                page <- toString <$> T.stripPrefix "page/" (toText fp)
                p <- Page <$> readMaybe page
                pure $ PaginatedRoute_OnPage p
        )
  routeUniverse m =
    -- TODO: How to tell the other pages to generate?
    PaginatedRoute_Main : fmap (PaginatedRoute_OnPage . Page) [2 .. (length m)]

data ListingRoute
  = ListingRoute_MultiVersion
  | ListingRoute_All (PaginatedRoute (NonEmpty Pkg))
  | ListingRoute_Broken (PaginatedRoute (NonEmpty Pkg))
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, IsRoute)
    via ( GenericRoute
            ListingRoute
            '[ WithModel [NonEmpty Pkg]
             , WithSubRoutes
                '[ FileRoute "index.html"
                 , FolderRoute "all" (PaginatedRoute (NonEmpty Pkg))
                 , FolderRoute "broken" (PaginatedRoute (NonEmpty Pkg))
                 ]
             ]
        )

-- | Like (==) but ignores the pagination
listingEq :: ListingRoute -> ListingRoute -> Bool
listingEq (ListingRoute_All _) (ListingRoute_All _) = True
listingEq (ListingRoute_Broken _) (ListingRoute_Broken _) = True
listingEq x y = x == y

instance HasSubModels ListingRoute where
  subModels m =
    SOP.I () -- (filter (\xs -> length xs > 1) m)
      SOP.:* SOP.I (pages m)
      SOP.:* SOP.I (pages $ filter (any (\Pkg {..} -> pname == name && broken)) m)
      SOP.:* SOP.Nil

data GhcRoute
  = GhcRoute_Index ListingRoute
  | GhcRoute_Package Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, IsRoute)
    via ( GenericRoute
            GhcRoute
            '[ WithModel (Map Text (NonEmpty Pkg))
             , WithSubRoutes
                '[ ListingRoute
                 , FolderRoute "p" (StringRoute (NonEmpty Pkg) Text)
                 ]
             ]
        )

instance HasSubModels GhcRoute where
  subModels m =
    SOP.I (Map.elems m) SOP.:* SOP.I m SOP.:* SOP.Nil

data HtmlRoute
  = HtmlRoute_GHC (Text, GhcRoute)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            HtmlRoute
            '[ WithModel NixData
             , WithSubRoutes
                '[ MapRoute Text GhcRoute
                 ]
             ]
        )

type StaticRoute = SR.StaticRoute "static"

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithModel Model
             , WithSubRoutes
                '[ HtmlRoute
                 , StaticRoute
                 ]
             ]
        )

-- TODO: upstream; https://github.com/EmaApps/ema/issues/144

{- | Like `FolderRoute` but using dynamic folder name, looked up on a `Map`.

  Empty folder name (map keys) are supported. They would translate in effect to the looked up route (no folder created).
-}
newtype MapRoute k r = MapRoute (k, r)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance (IsRoute r, IsString k, ToString k, Ord k, Show r) => IsRoute (MapRoute k r) where
  type RouteModel (MapRoute k r) = Map k (RouteModel r)
  routePrism :: (IsRoute r, IsString k) => RouteModel (MapRoute k r) -> Prism_ FilePath (MapRoute k r)
  routePrism rs =
    toPrism_ $
      prism'
        ( \(MapRoute (k, r)) ->
            let m = fromJust $ Map.lookup k rs -- HACK: fromJust
                prefix = if toString k == "" then "" else toString k <> "/"
             in prefix <> review (fromPrism_ $ routePrism @r m) r
        )
        ( \fp -> do
            let candidates =
                  case breakPath fp of
                    (a, Nothing) -> [("", a)]
                    (a, Just b) -> [(a, b), ("", fp)]
            (m, k, rest) <-
              asum $
                candidates <&> \(base, rest) ->
                  let k = fromString base
                   in (,k,rest) <$> Map.lookup k rs
            r <- preview (fromPrism_ $ routePrism @r m) (toString rest)
            pure $ MapRoute (k, r)
        )
    where
      -- Breaks a path once on the first slash.
      breakPath (s :: String) =
        case T.breakOn "/" (toText s) of
          (p, "") -> (toString p, Nothing)
          (p, toString -> '/' : rest) -> (toString p, Just rest)
          _ -> error "T.breakOn: impossible"

  routeUniverse rs = concatMap (\(a, m) -> MapRoute . (a,) <$> routeUniverse m) $ Map.toList rs
