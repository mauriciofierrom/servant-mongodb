{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module EmissionTypeSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec

import Lib
import Enterprise.API hiding (getEmissionTypes, getEmissionTypeById, addEmissionType, deleteEmissionType)
import Common.Types
import Enterprise.Types
import Test

getEmissionTypes :: ClientM [EmissionType]
getEmissionTypeById :: String -> ClientM EmissionType
addEmissionType :: EmissionType -> ClientM EmissionType
deleteEmissionType :: String -> ClientM NoContent

getEmissionTypes :<|> getEmissionTypeById :<|> addEmissionType :<|> deleteEmissionType = client emissionTypeApi

spec :: Spec
spec = beforeAll_ flushDB $
    describe "/emission-type" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "Allows to create a new Emission Type" $ \ env -> do
          _ <- try env $ addEmissionType (EmissionType "59884e90b75c5d1662000000" "New One" 3)
          emissionTypes <- try env getEmissionTypes
          isJust (find (\x -> etDescription x == "New One") emissionTypes) `shouldBe` True

        it "Allows to show Emission Types by id" $ \ env -> do
          emissionType <- try env (getEmissionTypeById "59884e90b75c5d1662000000")
          etDescription emissionType `shouldBe` "New One"

        it "Allows to update an Emission Type" $ \env -> do
          emissionType <- try env (getEmissionTypeById "59884e90b75c5d1662000000")
          _ <- try env $ addEmissionType $ EmissionType (etId emissionType) "Updated" (etCode emissionType)
          emissionTypes <- try env getEmissionTypes
          let exists = isJust (find (\x -> etDescription x == "Updated" && etCode x == 3) emissionTypes)
          let count = length $ filter (\x -> etDescription x == "Updated" && etCode x == 3) emissionTypes
          exists && (count == 1) `shouldBe` True

        it "Allows to delete an Emission Type" $ \env -> do
          emissionType <- try env (getEmissionTypeById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteEmissionType (etId emissionType)
          try env (getEmissionTypeById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "Throws a 404 error for missing Emission Types" $ \ env -> do
          try env (getEmissionTypeById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)
