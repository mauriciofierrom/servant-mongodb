{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module EnterpriseSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec

import Lib
import Enterprise.API hiding ( getEnterprises
                             , getEnterpriseById
                             , addEnterprise
                             , deleteEnterprise)
import Common.Types
import Enterprise.Types
import Test

getEnterprises :: ClientM [Enterprise]
getEnterpriseById :: String -> ClientM Enterprise
addEnterprise :: Enterprise -> ClientM Enterprise
deleteEnterprise :: String -> ClientM NoContent

getEnterprises :<|> getEnterpriseById :<|> addEnterprise :<|> deleteEnterprise = client enterpriseApi

spec :: Spec
spec = beforeAll_ flushDB $
  describe "/enterprise" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "Allows to create a new Enterprise" $ \env -> do
          _ <- try env $ addEnterprise (Enterprise "59884e90b75c5d1662000000" "New One" "0705277762" (IdentificationType "59884e90b75c5d1662000000" "RUC" 1) (EmissionType "59884e90b75c5d1662000000" "Normal" 1) (Environment "59884e90b75c5d1662000000" "Prueba" 2) "http://www.google.com" "Las Brisas" "072984480" "info@cafeinalab.com" 0 True [Matrix "59884e90b75c5d1662000000" "Matriz 1" 1])
          enterprises <- try env getEnterprises
          isJust (find (\x -> entName x == "New One") enterprises) `shouldBe` True

        it "Allows to show an Enterprise by id" $ \env -> do
          enterprise <- try env (getEnterpriseById "59884e90b75c5d1662000000")
          entName enterprise `shouldBe` "New One"

        it "Allows to update an Enterprise" $ \env -> do
          _ <- try env $ addEnterprise (Enterprise "59884e90b75c5d1662000000" "Updated" "0705277762" (IdentificationType "59884e90b75c5d1662000000" "RUC" 1) (EmissionType "59884e90b75c5d1662000000" "Normal" 1) (Environment "59884e90b75c5d1662000000" "Prueba" 2) "http://www.google.com" "Las Brisas" "072984480" "info@cafeinalab.com" 0 True [Matrix "59884e90b75c5d1662000000" "Matriz 1" 1])
          enterprises <- try env getEnterprises
          let exists = isJust (find (\x -> entName x == "Updated") enterprises)
          let count = length $ filter (\x -> entName x == "Updated") enterprises
          exists && (count == 1) `shouldBe` True

        it "Allows to delete an Enterprise" $ \env -> do
          enterprise <- try env (getEnterpriseById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteEnterprise (entId enterprise)
          try env (getEnterpriseById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "Throws a 404 error for missing items" $ \env -> do
          try env (getEnterpriseById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)
