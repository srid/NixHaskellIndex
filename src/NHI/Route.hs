{-# LANGUAGE UndecidableInstances #-}

module NHI.Route where

import Ema
import Ema.Route.Generic
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Ema.Route.Lib.Extra.StringRoute (StringRoute (StringRoute))
import Generics.SOP qualified as SOP
import NHI.Types (Pkg (..))

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
                 , FolderRoute "p" (StringRoute [Pkg] Text)
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
             , WithSubRoutes
                '[ HtmlRoute
                 , StaticRoute
                 ]
             ]
        )
