{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NHI.Site where

import Data.Aeson qualified as Aeson
import Data.Generics.Sum.Any (AsAny (_As))
import Ema (
  Asset (AssetGenerated),
  EmaSite (SiteArg, siteInput, siteOutput),
  Format (Html),
 )
import Ema.CLI qualified
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Language.Haskell.TH.Env (envQ')
import NHI.Route
import NHI.Types (NixData (..))
import NHI.View qualified as View
import Optics.Core (Prism', (%))
import Options.Applicative
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

instance EmaSite Route where
  type SiteArg Route = CliArgs
  siteInput cliAct args = do
    staticRouteDyn <- siteInput @StaticRoute cliAct ()
    liftIO (Aeson.eitherDecodeFileStrict' $$(envQ' "DATAFILE")) >>= \case
      Left err -> error $ toText err
      Right pkgs ->
        pure $ Model (cliArgsBaseUrl args) <$> staticRouteDyn <*> pure pkgs
  siteOutput rp m = \case
    Route_Html r ->
      pure $ Ema.AssetGenerated Ema.Html $ renderHtmlRoute rp m r
    Route_Static r ->
      siteOutput (rp % (_As @"Route_Static")) (modelStatic m) r

renderHtmlRoute :: Prism' FilePath Route -> Model -> HtmlRoute -> LByteString
renderHtmlRoute rp m r = do
  RU.renderHtml $ do
    H.docType
    H.html ! A.lang "en" $ do
      H.head $ do
        renderHead rp m r
      H.body ! A.class_ ("overflow-y-scroll " <> View.bodyBg) $ do
        renderBody rp m r

renderHead :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderHead rp model r = do
  H.meta ! A.charset "UTF-8"
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.title $ H.toHtml $ View.routeTitle r <> " - Nix Haskell Index"
  H.base ! A.href (H.toValue $ modelBaseUrl model)
  H.link ! A.rel "stylesheet" ! A.href (staticRouteUrl rp model "tailwind.css")

renderBody :: Prism' FilePath Route -> Model -> HtmlRoute -> H.Html
renderBody rp model r@(HtmlRoute_GHC (ghcVer, _)) = do
  H.div ! A.class_ "container mx-auto max-w-prose bg-white rounded shadow my-8" $ do
    View.renderNavbar rp model r
    H.h1 ! A.class_ (View.bodyBg <> " text-3xl font-bold text-white py-2 px-4") $ H.toHtml $ View.routeTitle r
    H.div ! A.class_ "px-4 py-4" $ do
      View.renderRoute rp (modelData model) r
  H.div ! A.class_ "px-4 my-4 flex items-center justify-center text-gray-50" $ do
    H.a ! A.href "https://ema.srid.ca" $ "Powered by Ema"

-- H.a ! A.href (staticRouteUrl rp model "logo.svg") $ do
--  H.img ! A.src (staticRouteUrl rp model "logo.svg") ! A.class_ "py-4 w-32" ! A.alt "Ema Logo"

routeLink :: Prism' FilePath Route -> HtmlRoute -> H.Html -> H.Html
routeLink rp r =
  H.a
    ! A.href (H.toValue $ View.routeUrl rp $ Route_Html r)
    ! A.class_ "text-rose-400"

-- | Link to a file under ./static
staticRouteUrl :: IsString r => Prism' FilePath Route -> Model -> FilePath -> r
staticRouteUrl rp m =
  SR.staticRouteUrl (rp % (_As @"Route_Static")) (modelStatic m)

-- CLI argument handling
-- ---------------------

data CliArgs = CliArgs
  { cliArgsBaseUrl :: Text
  , cliArgsEmaCli :: Ema.CLI.Cli
  }
  deriving stock (Eq, Show)

parseCliArgs :: IO CliArgs
parseCliArgs =
  execParser $ parserInfo cliParser
  where
    cliParser :: Parser CliArgs
    cliParser =
      CliArgs
        <$> option str (long "base-url" <> metavar "BASE_URL" <> help "Base URL to use in <base>" <> value "/")
        <*> Ema.CLI.cliParser
    parserInfo :: Parser a -> ParserInfo a
    parserInfo p =
      info
        (versionOption <*> p <**> helper)
        ( fullDesc
            <> progDesc "NixHaskellIndex: TODO"
            <> header "NixHaskellIndex"
        )
      where
        versionOption = infoOption "0.1" (long "version" <> help "Show version")
