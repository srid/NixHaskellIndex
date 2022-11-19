{-# LANGUAGE RecordWildCards #-}

module NHI.View where

import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ema qualified
import Ema.Route.Generic (HasSubModels (subModels))
import Ema.Route.Lib.Extra.PaginatedRoute (
  Page,
  PaginatedRoute,
  fromPage,
  getPage,
  lookupPage,
 )
import Generics.SOP (I (..), NP (..))
import NHI.Route
import NHI.Types
import Optics.Core
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

renderRoute :: Prism' FilePath Route -> NixData -> HtmlRoute -> H.Html
renderRoute rp NixData {..} = \case
  HtmlRoute_GHC (ghcVer, ghcRoute) -> do
    let pkgs = fromMaybe (error $ "Bad ghcVer lookup: " <> ghcVer) $ Map.lookup ghcVer packages
    renderGhcRoute rp pkgs nixpkgsRev (ghcVer, ghcRoute)

renderAbout :: Text -> Text -> H.Html
renderAbout nixpkgsRev ghcVer = do
  H.div ! A.class_ (bodyBg <> " text-gray-100 opacity-20 hover:opacity-100 text-xs border-2 border-gray-200 rounded shadow px-2 pb-2 mb-2") ! A.title "About this site" $ do
    H.p ! A.class_ "mt-2" $ do
      "Did you know that Haskell libraries on nixpkgs may have more than one version defined? And that the default or available versions do not necessarily correspond to that of Stackage LTS?"
    H.p ! A.class_ "mt-2" $ do
      H.b "This project is a WIP."
      " See the source "
      H.a ! A.class_ "underline" ! A.href "https://github.com/srid/NixHaskellIndex" $ "here"
      "."
    H.p ! A.class_ "mt-2" $ do
      "All data on this part of the site is based on "
      let url = "https://github.com/NixOS/nixpkgs/tree/" <> nixpkgsRev
      H.code $ H.a ! A.class_ "underline" ! A.href (H.toValue url) $ H.toHtml $ "github:NixOS/nixpkgs/" <> nixpkgsRev
      " as evaluated on "
      H.code "x86_64-linux" -- FIXME: make it dynamic
      " for the package set "
      H.code $ H.toHtml $ pkgSetForGhcVer ghcVer

renderGhcRoute :: Prism' FilePath Route -> Map Text (NonEmpty Pkg) -> Text -> (Text, GhcRoute) -> H.Html
renderGhcRoute rp pkgs nixpkgsRev (ghcVer, ghcRoute) = case ghcRoute of
  GhcRoute_Index lr -> do
    let I mMulti :* I mAll :* I mBroken :* Nil = subModels @ListingRoute $ Map.toList pkgs
        (pkgPages :: NonEmpty [(Text, NonEmpty Pkg)], pageR) = case lr of
          ListingRoute_MultiVersion pr ->
            (mMulti, pr)
          ListingRoute_All pr ->
            (mAll, pr)
          ListingRoute_Broken pr ->
            (mBroken, pr)
    H.div ! A.class_ "my-2 italic" $ do
      let numTotal = length pkgs
          numHere = sum $ length <$> pkgPages
      "Displaying "
      H.toHtml @Text $ show numHere <> " / " <> show numTotal <> " packages"
    case lr of
      ListingRoute_All r -> do
        renderPagination pkgPages r $ \p ->
          H.toValue $ routeUrl rp $ Route_Html $ HtmlRoute_GHC (ghcVer, GhcRoute_Index $ ListingRoute_All $ fromPage p)
      ListingRoute_Broken r -> do
        renderPagination pkgPages r $ \p ->
          H.toValue $ routeUrl rp $ Route_Html $ HtmlRoute_GHC (ghcVer, GhcRoute_Index $ ListingRoute_Broken $ fromPage p)
      _ ->
        mempty
    forM_ (fromMaybe (error "No pages!") $ lookupPage pageR pkgPages) $ \(k, vers) -> do
      H.div $ do
        H.header ! A.class_ "font-bold text-xl mt-4 hover:underline" $
          H.a ! A.href (H.toValue $ routeUrl rp $ Route_Html $ HtmlRoute_GHC (ghcVer, GhcRoute_Package k)) $
            H.toHtml k
        renderVersions k vers
  GhcRoute_Package name -> do
    let vers = fromMaybe (error $ "Bad package lookup: " <> name) $ Map.lookup name pkgs
    H.div ! A.class_ "mt-4" $ do
      renderVersions name vers
    H.div ! A.class_ "mt-8 prose" $ do
      H.h2 "Inspect in `nix repl`"
      H.pre ! A.class_ "bg-gray-700 text-white p-2 my-2 rounded" $ do
        H.code $ do
          H.toHtml @Text $ "$ nix repl github:NixOS/nixpkgs/" <> nixpkgsRev <> "\n"
          H.toHtml @Text $ "nix-repl> pkgs = legacyPackages.${builtins.currentSystem}\n"
          H.toHtml @Text $ "nix-repl> " <> pkgSetForGhcVer ghcVer <> "." <> name <> "  # Hit <tab> here to autocomplete versions\n"
          H.toHtml @Text "«derivation /nix/store/???.drv»"

renderPagination :: NonEmpty [a] -> PaginatedRoute a -> (Page -> H.AttributeValue) -> H.Html
renderPagination xs pageR pageUrl =
  H.div ! A.class_ "flex flex-row text-sm space-x-1 text-blue-500" $ do
    forM_ [1 :: Page .. (fromInteger . toInteger $ length xs)] $ \i -> do
      if i == getPage pageR
        then H.b $ H.toHtml @Text $ show i
        else H.a ! A.class_ "hover:underline opacity-50 hover:opacity-100" ! A.href (pageUrl i) $ H.toHtml @Text $ show i

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

bodyBg :: H.AttributeValue
bodyBg = "bg-gray-700"

renderNavbar :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderNavbar rp Model {..} (HtmlRoute_GHC (k0, subRoute0)) =
  H.div ! A.class_ bodyBg $
    H.div ! A.class_ "w-full flex bg-pink-200 rounded-t-xl shadow-t items-center justify-center" $ do
      H.nav ! A.class_ "text-xs rounded-t font-bold flex flex-col items-center justify-center" $ do
        let navRoutes :: [Text] = Map.keys (packages modelData)
        H.div ! A.class_ "flex flex-row space-x-4 mb-1" $ do
          forM_ navRoutes $ \k ->
            let extraClass = if k == k0 then "bg-rose-400 text-white" else "text-gray-700"
                r = HtmlRoute_GHC (k, GhcRoute_Index def)
             in H.a
                  ! A.href (H.toValue $ routeUrl rp $ Route_Html r)
                  ! A.class_ ("p-2 " <> extraClass)
                  $ H.toHtml (if k == "" then "default" else k)
        let navSubRoutes :: [ListingRoute] = [ListingRoute_All def, ListingRoute_MultiVersion def, ListingRoute_Broken def]
        H.div ! A.class_ "flex flex-row space-x-4" $ do
          forM_ navSubRoutes $ \lR ->
            let same = case subRoute0 of
                  GhcRoute_Index x -> listingEq x lR
                  GhcRoute_Package _ -> case lR of
                    ListingRoute_All _ -> True
                    _ -> False
                extraClass = if same then "bg-rose-400 text-white" else "text-gray-700"
                gr = GhcRoute_Index lR
                r = HtmlRoute_GHC (k0, GhcRoute_Index lR)
             in H.a
                  ! A.href (H.toValue $ routeUrl rp $ Route_Html r)
                  ! A.class_ ("p-2 " <> extraClass)
                  $ H.toHtml (routeTitle' gr)

routeTitle :: HtmlRoute -> Text
routeTitle (HtmlRoute_GHC (ver, r)) =
  (<> ghcVerSuffix ver) $ routeTitle' r

routeTitle' :: GhcRoute -> Text
routeTitle' r =
  case r of
    GhcRoute_Index lr ->
      let page = listingRoutePage lr
       in (if page == 1 then "" else "Page " <> show page <> " of ") <> case lr of
            ListingRoute_MultiVersion _ -> "Multi-version packages"
            ListingRoute_All _ -> "All packages"
            ListingRoute_Broken _ -> "Broken packages"
    GhcRoute_Package pname -> pname

ghcVerSuffix :: Text -> Text
ghcVerSuffix ghcVer =
  if ghcVer == "" then "" else " - GHC " <> ghcVer

pkgSetForGhcVer :: Text -> Text
pkgSetForGhcVer ghcVer =
  if ghcVer == "" then "pkgs.haskellPackages" else "pkgs.haskell.packages.ghc" <> T.replace "." "" ghcVer

routeUrl :: forall {r}. Prism' FilePath r -> r -> Text
routeUrl = Ema.routeUrlWith Ema.UrlPretty
