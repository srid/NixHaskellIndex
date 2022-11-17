module Main where

import Ema qualified
import NHI.Route (Route)
import NHI.Site qualified as Site

-- Main entrypoint
-- ---------------

main :: IO ()
main = do
  cliArgs <- Site.parseCliArgs
  void $ Ema.runSiteWithCli @Route (Site.cliArgsEmaCli cliArgs) cliArgs
