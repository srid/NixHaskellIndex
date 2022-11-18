{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module NHI.Route where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
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

data ListingRoute
  = ListingRoute_MultiVersion
  | ListingRoute_All
  | ListingRoute_Broken
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            ListingRoute
            '[ WithModel ()
             , WithSubRoutes
                '[ FileRoute "index.html"
                 , FileRoute "all.html"
                 , FileRoute "broken.html"
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

data GhcRoute
  = GhcRoute_Index ListingRoute
  | GhcRoute_Package Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            GhcRoute
            '[ WithModel (Map Text (NonEmpty Pkg))
             , WithSubRoutes
                '[ ListingRoute
                 , FolderRoute "p" (StringRoute (NonEmpty Pkg) Text)
                 ]
             ]
        )

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
