{-# LANGUAGE RecordWildCards #-}

module NHI.View where

import Data.Map.Strict qualified as Map
import NHI.Route
import NHI.Types
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

renderRoute :: Map Text [Pkg] -> HtmlRoute -> H.Html
renderRoute packages = \case
  HtmlRoute_Index lr -> do
    let pkgs' = case lr of
          ListingRoute_All -> packages
          ListingRoute_MultiVersion -> Map.filter (\xs -> length xs > 1) packages
    H.div ! A.class_ "bg-red-200 p-2 m-2" $ do
      H.header $ H.text "WARNING: This site is a WIP"
    forM_ (Map.toList pkgs') $ \(k, vers) -> do
      H.div $ do
        H.header ! A.class_ "font-bold text-xl mt-2" $ H.toHtml k
        forM_ vers $ \Pkg {..} -> do
          H.li $ do
            H.code (H.toHtml name) <> " (" <> H.toHtml version <> ")"
  HtmlRoute_About -> do
    "WIP: https://github.com/srid/NixHaskellIndex"
