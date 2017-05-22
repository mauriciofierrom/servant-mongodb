{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module IdentificationTypeSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec

import Lib
import Enterprise.API hiding (getIdentificationTypes, getIdentificationTypeById, addIdentificationType, deleteIdentificationType)
import Common.Types
import Enterprise.Types
import Test

getIdentificationTypes :: ClientM [IdentificationType]
getIdentificationTypeById :: String -> ClientM IdentificationType
addIdentificationType :: IdentificationType -> ClientM IdentificationType
deleteIdentificationType :: String -> ClientM NoContent

getIdentificationTypes :<|> getIdentificationTypeById :<|> addIdentificationType :<|> deleteIdentificationType = client identificationTypeApi

spec :: Spec
spec = beforeAll_ flushDB $
    describe "/identification-type" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "Allows to create a new Identification Type" $ \ env -> do
          _ <- try env $ addIdentificationType (IdentificationType "59884e90b75c5d1662000000" "New One" 3)
          identificationTypes <- try env getIdentificationTypes
          isJust (find (\x -> itDescription x == "New One") identificationTypes) `shouldBe` True

        it "Allows to show Identification Types by id" $ \ env -> do
          identificationType <- try env (getIdentificationTypeById "59884e90b75c5d1662000000")
          itDescription identificationType `shouldBe` "New One"

        it "Allows to update an Identification Type" $ \env -> do
          identificationType <- try env (getIdentificationTypeById "59884e90b75c5d1662000000")
          _ <- try env $ addIdentificationType $ IdentificationType (itId identificationType) "Updated" (itCode identificationType)
          identificationTypes <- try env getIdentificationTypes
          let exists = isJust (find (\x -> itDescription x == "Updated" && itCode x == 3) identificationTypes)
          let count = length $ filter (\x -> itDescription x == "Updated" && itCode x == 3) identificationTypes
          exists && (count == 1) `shouldBe` True

        it "Allows to delete an Identification Type" $ \env -> do
          identificationType <- try env (getIdentificationTypeById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteIdentificationType (itId identificationType)
          try env (getIdentificationTypeById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "Throws a 404 error for missing Identification Types" $ \ env -> do
          try env (getIdentificationTypeById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)
