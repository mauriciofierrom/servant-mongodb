{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ClientSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client hiding ( Client )
import Test.Hspec

import Lib
import Bill.API hiding ( getClients, getClientById, addClient, deleteClient )
import Bill.Types
import Enterprise.Types
import Common.Types
import Test

getClients :: ClientM [Client]
getClientById :: String -> ClientM Client
addClient :: Client -> ClientM Client
deleteClient :: String -> ClientM NoContent

getClients :<|> getClientById :<|> addClient :<|> deleteClient = client clientApi

spec :: Spec
spec = beforeAll_ flushDB $
    describe "/client" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "Allows to create a new Client" $ \ env -> do
          _ <- try env $ addClient (Client "59884e90b75c5d1662000000" "New One" "Last Name" (IdentificationType "59884e90b75c5d1662000000" "IdentificationType" 2) "0705277762" "Adddress" "2984480" "email@somewhere.com")
          clients <- try env getClients
          isJust (find (\x -> clientName x == "New One") clients) `shouldBe` True

        it "Allows to show Client by id" $ \ env -> do
          client <- try env (getClientById "59884e90b75c5d1662000000")
          clientName client `shouldBe` "New One"

        it "Allows to update a Client" $ \env -> do
          _ <- try env $ addClient $ Client "59884e90b75c5d1662000000" "Updated" "Last Name" (IdentificationType "59884e90b75c5d1662000000" "IdentificationType" 2) "0705277762" "Adddress" "2984480" "email@somewhere.com"
          clients <- try env getClients
          let exists = isJust (find (\x -> clientName x == "Updated") clients)
          let count = length $ filter (\x -> clientName x == "Updated") clients
          exists && (count == 1) `shouldBe` True

        it "Allows to delete a Client" $ \env -> do
          client <- try env (getClientById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteClient (clientId client)
          try env (getClientById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "Throws a 404 error for missing Clients" $ \ env -> do
          try env (getClientById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)
