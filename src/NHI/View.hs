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

renderRoute :: Prism' FilePath Route -> NixData -> HtmlRoute -> H.Html
renderRoute rp NixData {..} = \case
  HtmlRoute_GHC (ghcVer, ghcRoute) -> do
    renderAbout nixpkgsRev ghcVer
    let pkgs = fromJust $ Map.lookup ghcVer packages
    renderGhcRoute rp pkgs nixpkgsRev (ghcVer, ghcRoute)

renderAbout :: Text -> Text -> H.Html
renderAbout nixpkgsRev ghcVer = do
  H.div ! A.class_ "text-xs bg-purple-50 border-2 border-gray-200 rounded shadow px-2 pb-2" $ do
    H.p ! A.class_ "mt-2" $ do
      "Did you know that Haskell libraries on nixpkgs may have more than one version defined? And that the default or available versions do not necessarily correspond to that of Stackage LTS?"
    H.p ! A.class_ "mt-2" $ do
      H.b "This project is a WIP."
      " See the source "
      H.a ! A.class_ "underline" ! A.href "https://github.com/srid/NixHaskellIndex" $ "here"
      "."
    H.p ! A.class_ "mt-2" $ do
      "All data on this site is based on "
      let url = "https://github.com/NixOS/nixpkgs/tree/" <> nixpkgsRev
      H.a ! A.class_ "underline" ! A.href (H.toValue url) $ H.toHtml $ "github:NixOS/nixpkgs/" <> nixpkgsRev
      " as evaluated on x86_64-linux."
    H.p ! A.class_ "mt-2" $ do
      "You are viewing GHC version: "
      H.toHtml $ if ghcVer == "" then "default (pkgs.haskellPackages)" else ghcVer

renderGhcRoute :: Prism' FilePath Route -> Map Text (NonEmpty Pkg) -> Text -> (Text, GhcRoute) -> H.Html
renderGhcRoute rp pkgs nixpkgsRev (ghcVer, ghcRoute) = case ghcRoute of
  GhcRoute_Index lr -> do
    let numTotal = length pkgs
        numHere = length pkgs'
        pkgs' = case lr of
          ListingRoute_All -> pkgs
          ListingRoute_MultiVersion ->
            Map.filter (\xs -> length xs > 1) pkgs
          ListingRoute_Broken ->
            Map.filter (any (\Pkg {..} -> pname == name && broken)) pkgs
    H.b $ H.toHtml @Text $ show numHere <> " / " <> show numTotal <> " packages"
    forM_ (Map.toList pkgs') $ \(k, vers) -> do
      H.div $ do
        H.header ! A.class_ "font-bold text-xl mt-4 hover:underline" $
          H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html $ HtmlRoute_GHC (ghcVer, GhcRoute_Package k)) $
            H.toHtml k
        renderVersions k vers
  GhcRoute_Package name -> do
    let vers = fromJust $ Map.lookup name pkgs
    H.div ! A.class_ "mt-4" $ do
      renderVersions name vers
    H.div ! A.class_ "mt-8 prose" $ do
      H.h2 "Inspect in `nix repl`"
      H.pre ! A.class_ "bg-gray-700 text-white p-2 my-2 rounded" $ do
        H.code $ do
          H.toHtml @Text $ "$ nix repl github:NixOS/nixpkgs/" <> nixpkgsRev <> "\n"
          H.toHtml @Text $ "nix-repl> pkgs = legacyPackages.${builtins.currentSystem}\n"
          H.toHtml @Text $ "nix-repl> pkgs.haskellPackages." <> name <> "  # Hit <tab> here to autocomplete versions\n"
          H.toHtml @Text "«derivation /nix/store/???.drv»"

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

renderNavbar :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderNavbar rp Model {..} (HtmlRoute_GHC (k0, subRoute0)) =
  H.div ! A.class_ "w-full flex items-center justify-center" $ do
    H.nav ! A.class_ "text-xs bg-blue-50 rounded-t font-bold flex flex-col items-center justify-center  mb-4" $ do
      let navRoutes :: [Text] = Map.keys (packages modelData)
      H.div ! A.class_ "flex flex-row space-x-4 " $ do
        forM_ navRoutes $ \k ->
          let extraClass = if k == k0 then "bg-rose-400 text-white" else "text-gray-700"
              r = HtmlRoute_GHC (k, GhcRoute_Index ListingRoute_MultiVersion)
           in H.a
                ! A.href (H.toValue $ routeUrl rp $ Route_Html r)
                ! A.class_ ("p-2 " <> extraClass)
                $ H.toHtml (if k == "" then "default" else k)
      let navSubRoutes :: [ListingRoute] = universe
      H.div ! A.class_ "flex flex-row space-x-4 " $ do
        forM_ navSubRoutes $ \lR ->
          let extraClass = if GhcRoute_Index lR == subRoute0 then "bg-rose-400 text-white" else "text-gray-700"
              r = HtmlRoute_GHC (k0, GhcRoute_Index lR)
           in H.a
                ! A.href (H.toValue $ routeUrl rp $ Route_Html r)
                ! A.class_ ("p-2 " <> extraClass)
                $ H.toHtml (routeTitle r)

routeTitle :: HtmlRoute -> Text
routeTitle r = case r of
  HtmlRoute_GHC (ver, GhcRoute_Index ListingRoute_All) -> "All packages"
  HtmlRoute_GHC (ver, GhcRoute_Index ListingRoute_MultiVersion) -> "Packages with more than one version"
  HtmlRoute_GHC (ver, GhcRoute_Index ListingRoute_Broken) -> "Broken packages"
  HtmlRoute_GHC (ver, GhcRoute_Package pname) -> pname

routeUrl :: forall {r}. Prism' FilePath r -> r -> Text
routeUrl = Ema.routeUrlWith Ema.UrlPretty
