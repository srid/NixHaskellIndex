{-# LANGUAGE UndecidableInstances #-}

module NHI.Route where

import Data.Map.Strict qualified as Map
import Ema
import Ema.Route.Generic
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Ema.Route.Prism
import Generics.SOP qualified as SOP
import NHI.Types (Pkg (..))
import Optics.Core

data Model = Model
  { modelBaseUrl :: Text
  , modelStatic :: SR.Model
  , modelPackages :: Map Text [Pkg]
  }
  deriving stock (Eq, Show, Generic)

data ListingRoute
  = ListingRoute_MultiVersion
  | ListingRoute_All
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
                 ]
             ]
        )

-- | A route represented by a stringy type; associated with a foldable of the same as its model.
newtype StringRoute (a :: Type) r = StringRoute {unStringRoute :: r}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance (IsString r, ToString r, Eq r, Ord r) => IsRoute (StringRoute a r) where
  type RouteModel (StringRoute a r) = Map r a
  routePrism as =
    toPrism_ $
      htmlSuffixPrism
        % iso fromString toString
        % mapMemberPrism as
        % coercedTo
    where
      mapMemberPrism m =
        prism' id $ \r -> r <$ guard (r `Map.member` m)
  routeUniverse as = StringRoute <$> Map.keys as

type PackageRoute = StringRoute [Pkg] Text

data HtmlRoute
  = HtmlRoute_Index ListingRoute
  | HtmlRoute_Package Text
  | HtmlRoute_About
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            HtmlRoute
            '[ WithModel (Map Text [Pkg])
             , WithSubRoutes
                '[ ListingRoute
                 , FolderRoute "p" PackageRoute
                 , FileRoute "about.html"
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
             , WithSubModels
                '[ Map Text [Pkg]
                 , SR.Model
                 ]
             , WithSubRoutes
                '[ HtmlRoute
                 , StaticRoute
                 ]
             ]
        )
