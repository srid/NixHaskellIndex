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

renderRoute :: Prism' FilePath Route -> Map Text (NonEmpty Pkg) -> HtmlRoute -> H.Html
renderRoute rp packages = \case
  HtmlRoute_Index lr -> do
    let pkgs' = case lr of
          ListingRoute_All -> packages
          ListingRoute_MultiVersion ->
            Map.filter (\xs -> length xs > 1) packages
          ListingRoute_Broken ->
            Map.filter (any (\Pkg {..} -> broken)) packages
    forM_ (Map.toList pkgs') $ \(k, vers) -> do
      H.div $ do
        H.header ! A.class_ "font-bold text-xl mt-4 hover:underline" $
          H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html $ HtmlRoute_Package k) $
            H.toHtml k
        renderVersions k vers
  HtmlRoute_Package name -> do
    let vers = fromJust $ Map.lookup name packages
    H.div $ do
      renderVersions name vers
  HtmlRoute_About -> do
    H.p ! A.class_ "mt-2" $ do
      "Did you know that Haskell libraries on nixpkgs may have more than one version defined? And that the default or available versions do not necessarily correspond to that of Stackage LTS?"
    H.p ! A.class_ "mt-2" $ do
      H.b "This project is a WIP."
      " See the source "
      H.a ! A.class_ "underline" ! A.href "https://github.com/srid/NixHaskellIndex" $ "here"
      "."

renderVersions :: Text -> NonEmpty Pkg -> H.Html
renderVersions k vers =
  H.div ! A.class_ "flex flex-col" $ do
    forM_ vers $ \Pkg {..} -> do
      H.div ! A.class_ "py-0.5 flex flex-row space-x-2" $ do
        H.code (H.toHtml name)
        H.span ! A.class_ "bg-purple-100 font-mono small rounded" $ do
          H.toHtml version
        when (length vers > 1 && k == name) $ do
          H.span ! A.class_ "bg-green-200 px-0.5 font-bold small rounded" $ do
            "default"
        when broken $ do
          H.span ! A.class_ "bg-red-200 px-0.5 font-bold small rounded" $ do
            "broken"

routeUrl :: forall {r}. Prism' FilePath r -> r -> Text
routeUrl = Ema.routeUrlWith Ema.UrlPretty
