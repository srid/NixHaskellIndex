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

data NixData = NixData
  { nixpkgsRev :: Text
  , packages :: Map Text (Map Text (NonEmpty Pkg))
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
