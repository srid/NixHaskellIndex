{-# LANGUAGE RecordWildCards #-}

module NHI.View where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Ema qualified
import NHI.Route
import NHI.Types
import Optics.Core
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

renderRoute :: Prism' FilePath Route -> Map Text [Pkg] -> HtmlRoute -> H.Html
renderRoute rp packages = \case
  HtmlRoute_Index lr -> do
    let pkgs' = case lr of
          ListingRoute_All -> packages
          ListingRoute_MultiVersion -> Map.filter (\xs -> length xs > 1) packages
    H.div ! A.class_ "bg-red-200 p-2 m-2" $ do
      H.header $ H.text "WARNING: This site is a WIP"
    forM_ (Map.toList pkgs') $ \(k, vers) -> do
      H.div $ do
        H.header ! A.class_ "font-bold text-xl mt-2" $
          H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html $ HtmlRoute_Package k) $
            H.toHtml k
        forM_ vers $ \Pkg {..} -> do
          H.li $ do
            H.code (H.toHtml name) <> " (" <> H.toHtml version <> ")"
  HtmlRoute_Package name -> do
    let vers = fromJust $ Map.lookup name packages
    H.div $ do
      H.header ! A.class_ "font-bold text-xl mt-2" $
        H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html $ HtmlRoute_Package name) $
          H.toHtml name
      forM_ vers $ \Pkg {..} -> do
        H.li $ do
          H.code (H.toHtml name) <> " (" <> H.toHtml version <> ")"
  HtmlRoute_About -> do
    "WIP: https://github.com/srid/NixHaskellIndex"

routeUrl :: forall {r}. Prism' FilePath r -> r -> Text
routeUrl = Ema.routeUrlWith Ema.UrlPretty