{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bill.Types where

import GHC.Generics
import Data.Aeson
import Data.Time

import Enterprise.Types ( IdentificationType )
import Item.Types ( Item )

data Client =
  Client { clientId                 :: String
         , clientName               :: String
         , clientLastName           :: String
         , clientIdentificationType :: IdentificationType
         , clientIdentification     :: String
         , clientAddress            :: String
         , clientPhone              :: String
         , clientEmail              :: String }
         deriving (Eq, Show, Generic)

instance ToJSON Client
instance FromJSON Client

data Bill =
  Bill { billId     :: String
       , billNumber :: Integer
       , billDate   :: UTCTime
       , billClient :: Client
       , billDetail :: [BillDetail] }
         deriving (Eq, Show, Generic)

instance ToJSON Bill
instance FromJSON Bill

data BillDetail =
  BillDetail { bdId       :: String
             , bdItem     :: Item
             , bdQuantity :: Integer }
         deriving (Eq, Show, Generic)

instance ToJSON BillDetail
instance FromJSON BillDetail
