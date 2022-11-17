module NHI.Types where

import Data.Aeson

data Pkg = Pkg
  { name :: Text
  , pname :: Text
  , version :: Text
  , broken :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
