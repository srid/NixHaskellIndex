{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module NHI.PaginatedRoute where

import Data.Default (Default (..))
import Data.Text qualified as T
import Ema
import Generics.SOP qualified as SOP
import Optics.Core (prism')

newtype Page = Page {unPage :: Int}
  deriving newtype (Show, Eq, Ord, Num, Enum)

data PaginatedRoute (t :: Type) = PaginatedRoute_Main | PaginatedRoute_OnPage Page
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Default (PaginatedRoute t) where
  def = PaginatedRoute_Main

getPage :: forall {t}. PaginatedRoute t -> Page
getPage = \case
  PaginatedRoute_Main -> Page 1
  PaginatedRoute_OnPage p -> p

fromPage :: forall {t}. Page -> PaginatedRoute t
fromPage = \case
  Page 1 -> PaginatedRoute_Main
  p -> PaginatedRoute_OnPage p

lookupPage :: PaginatedRoute a -> [[a]] -> Maybe [a]
lookupPage r xs =
  xs !!? (unPage (getPage r) - 1)

instance IsRoute (PaginatedRoute a) where
  type RouteModel (PaginatedRoute a) = [[a]]
  routePrism m =
    toPrism_ $
      prism'
        ( \case
            PaginatedRoute_Main -> "index.html"
            PaginatedRoute_OnPage page -> "page/" <> show (unPage page) <> ".html"
        )
        ( \fp -> do
            if fp == "index.html"
              then pure PaginatedRoute_Main
              else do
                page <- fmap toString $ T.stripSuffix ".html" =<< T.stripPrefix "page/" (toText fp)
                p <- Page <$> readMaybe page
                void $ lookupPage (fromPage p) m -- Check if this page exists
                pure $ PaginatedRoute_OnPage p
        )
  routeUniverse m =
    -- TODO: How to tell the other pages to generate?
    PaginatedRoute_Main : fmap (PaginatedRoute_OnPage . Page) [2 .. (length m)]
