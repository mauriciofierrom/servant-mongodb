{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Enterprise.Types
  ( Enterprise (..)
  , Environment (..)
  , IdentificationType (..)
  , EmissionType (..)
  , Matrix (..) )
where

import GHC.Generics
import Data.Aeson

data IdentificationType =
  IdentificationType { itId   :: String
                     , itDescription :: String
                     , itCode :: Int}
                     deriving (Eq, Show, Generic)

instance FromJSON IdentificationType
instance ToJSON IdentificationType

data EmissionType =
  EmissionType { etId   :: String
               , etDescription :: String
               , etCode :: Int}
               deriving (Eq, Show, Generic)

instance FromJSON EmissionType
instance ToJSON EmissionType

data Matrix =
  Matrix { matrixId :: String
         , matrixDescription :: String
         , matrixNumber :: Int }
        deriving (Eq, Show, Generic)

instance FromJSON Matrix
instance ToJSON Matrix

data Enterprise =
  Enterprise { entId                 :: String
             , entName               :: String
             , entIdentification     :: String
             , entIdentificationType :: IdentificationType
             , entEmissionType       :: EmissionType
             , entEnvironment        :: Environment
             , entP12Url             :: String
             , entAddress            :: String
             , entTelephone          :: String
             , entEmail              :: String
             , entSpecialContributor :: Integer
             , entAccountingObliged  :: Bool
             , entMatrixes           :: [Matrix] }
             deriving (Eq, Show, Generic)

instance FromJSON Enterprise
instance ToJSON Enterprise

data Environment = Environment {
  envId          :: String
, envDescription :: String
, envCode        :: Integer
}
  deriving (Eq, Show, Generic)

instance FromJSON Environment
instance ToJSON Environment
