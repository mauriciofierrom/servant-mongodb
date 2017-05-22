{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Item.Types
  ( ItemType (..)
  , Item (..)
  , Tax (..)
  , TaxRate (..))
where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)

data ItemType = ItemType {
  itemTypeId          :: String
, itemTypeDescription :: String
}
  deriving (Eq, Show, Generic)

instance FromJSON ItemType
instance ToJSON ItemType

data Item =
  Item { itemId          :: String
       , itemDescription :: Text
       , itemTax         :: Tax
       , itemCode        :: Integer
       , itemUnitValue   :: Double
       , itemDiscount    :: Integer
       , itemTaxRate     :: TaxRate
       , itemType        :: ItemType }
       deriving (Eq, Show, Generic)

instance FromJSON Item
instance ToJSON Item

data Tax = Tax {
  taxId   :: String
, taxName :: String
, taxCode :: Integer
} deriving (Eq, Show, Generic)

instance FromJSON Tax
instance ToJSON Tax

data TaxRate =
  TaxRate { taxRateId           :: String
          , taxRateCode         :: Integer
          , taxRateDescription  :: String
          , taxRateGroup        :: Integer
          , taxRateTax          :: String
          , taxRatePercentage   :: Maybe Integer
          , taxRateRate         :: Maybe Float
          , taxRateUnit         :: Maybe Integer}
          deriving (Eq, Show, Generic)

instance FromJSON TaxRate
instance ToJSON TaxRate
