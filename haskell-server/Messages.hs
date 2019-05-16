{-# LANGUAGE DeriveGeneric #-}

module Messages (
  Message (..)  
) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (sumEncoding, Options, SumEncoding(ObjectWithSingleField))

data Message = CellUpdate {
  x :: Char,
  y :: Int,
  value :: Either String (Maybe Integer)
} | FormulaUpdate {
  x :: Char,
  y :: Int,
  formula :: Maybe String
} deriving (Generic)


----------------------
-- JSON En/Decoding --

jsonOptions :: Options
jsonOptions = defaultOptions { sumEncoding = ObjectWithSingleField }

instance ToJSON Message where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Message where
  parseJSON = genericParseJSON jsonOptions
  