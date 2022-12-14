{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module NHI.Route where

import Data.Default (Default (..))
import Data.Map.Strict qualified as Map
import Data.Sequence (chunksOf)
import Data.Sequence qualified as Seq
import Ema (IsRoute)
import Ema.Route.Generic
import Ema.Route.Lib.Extra.MapRoute (MapRoute (..))
import Ema.Route.Lib.Extra.PaginatedRoute (Page)
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Ema.Route.Lib.Extra.StringRoute (StringRoute (StringRoute))
import Generics.SOP qualified as SOP
import NHI.Types (NixData, Pkg (..))

data Model = Model
  { modelBaseUrl :: Text
  , modelStatic :: SR.Model
  , modelData :: NixData
  }
  deriving stock (Eq, Show, Generic)

type PkgsetPage = Page (Text, NonEmpty Pkg)

data ListingRoute
  = ListingRoute_MultiVersion PkgsetPage
  | ListingRoute_All PkgsetPage
  | ListingRoute_Broken PkgsetPage
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, IsRoute)
    via ( GenericRoute
            ListingRoute
            '[ WithModel [(Text, NonEmpty Pkg)]
             , WithSubRoutes
                '[ FolderRoute "multi" PkgsetPage
                 , PkgsetPage
                 , FolderRoute "broken" PkgsetPage
                 ]
             ]
        )

instance Default ListingRoute where
  def = ListingRoute_All def

listingRoutePage :: ListingRoute -> PkgsetPage
listingRoutePage = \case
  ListingRoute_MultiVersion r -> r
  ListingRoute_All r -> r
  ListingRoute_Broken r -> r

-- | Like (==) but ignores the pagination
listingEq :: ListingRoute -> ListingRoute -> Bool
listingEq (ListingRoute_All _) (ListingRoute_All _) = True
listingEq (ListingRoute_Broken _) (ListingRoute_Broken _) = True
listingEq x y = x == y

instance HasSubModels ListingRoute where
  subModels m =
    SOP.I (pages $ filter (\(_, xs) -> length xs > 1) m)
      SOP.:* SOP.I (pages m)
      SOP.:* SOP.I (pages $ filter (\(_, v) -> any (\Pkg {..} -> pname == name && broken) v) m)
      SOP.:* SOP.Nil
    where
      pages :: [a] -> NonEmpty [a]
      pages xs = fromMaybe (one mempty) . nonEmpty . fmap toList . toList $ chunksOf pageSize (Seq.fromList xs)
        where
          pageSize :: Int
          pageSize = 500

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
    SOP.I (Map.toList m) SOP.:* SOP.I m SOP.:* SOP.Nil

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
