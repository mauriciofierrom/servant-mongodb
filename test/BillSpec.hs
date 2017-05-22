{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module BillSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec
import Data.Time.Clock ( getCurrentTime )

import Lib
import Bill.API hiding ( getBills, getBillById, addBill, deleteBill )
import Bill.Types
import Enterprise.Types
import Common.Types
import Test

getBills :: ClientM [Bill]
getBillById :: String -> ClientM Bill
addBill :: Bill -> ClientM Bill
deleteBill :: String -> ClientM NoContent

getBills :<|> getBillById :<|> addBill :<|> deleteBill = client billApi

spec :: Spec
spec = beforeAll_ flushDB $
    describe "/bill" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "Allows to create a new Bill" $ \ env -> do
          date <- liftIO getCurrentTime
          let client' = (Client "59884e90b75c5d1662000000" "New One" "Last Name" (IdentificationType "59884e90b75c5d1662000000" "IdentificationType" 2) "0705277762" "Adddress" "2984480" "email@somewhere.com")

          _ <- try env $ addBill (Bill "59884e90b75c5d1662000000" 123 date client' [])
          bills <- try env getBills
          isJust (find (\x -> billNumber x == 123) bills) `shouldBe` True

        it "Allows to show Bills by id" $ \ env -> do
          bill <- try env (getBillById "59884e90b75c5d1662000000")
          billNumber bill `shouldBe` 123

        it "Allows to update a Bill" $ \env -> do
          bill <- try env (getBillById "59884e90b75c5d1662000000")
          _ <- try env $ addBill $ Bill (billId bill) 321 (billDate bill) (billClient bill) (billDetail bill)
          bills <- try env getBills
          let exists = isJust (find (\x -> billNumber x == 321) bills)
          let count = length $ filter (\x -> billNumber x == 321) bills
          exists && (count == 1) `shouldBe` True

        it "Allows to delete a Bill" $ \env -> do
          bill <- try env (getBillById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteBill (billId bill)
          try env (getBillById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "Throws a 404 error for missing Bills" $ \ env -> do
          try env (getBillById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

