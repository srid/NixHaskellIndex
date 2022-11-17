module NHI.Types where

import Data.Aeson

data Pkg = Pkg {name :: Text, pname :: Text, version :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
