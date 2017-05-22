{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ItemSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec

import Lib
import Item.API hiding ( getItems, getItemById, addItem, deleteItem )
import Item.Types
import Common.Types
import Test

getItems :: ClientM [Item]
getItemById :: String -> ClientM Item
addItem :: Item -> ClientM Item
deleteItem :: String -> ClientM NoContent

getItems :<|> getItemById :<|> addItem :<|> deleteItem = client itemApi

spec :: Spec
spec = beforeAll_ flushDB $
    describe "/item" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "Allows to create a new Item" $ \ env -> do
          _ <- try env $ addItem (Item "59884e90b75c5d1662000000" "New One" (Tax "5989a7da5d37b91d79c07532" "Tax" 2) 2 2.5 10 (TaxRate "5989a7da5d37b91d79c07551" 2 "TaxRate" 2 "59884e90b75c5d1662000000" Nothing Nothing Nothing) (ItemType "59884e90b75c5d1662000000" "ItemType"))
          items <- try env getItems
          isJust (find (\x -> itemDescription x == "New One") items) `shouldBe` True

        it "Allows to show Item by id" $ \ env -> do
          item <- try env (getItemById "59884e90b75c5d1662000000")
          itemDescription item `shouldBe` "New One"

        it "Allows to update a Item" $ \env -> do
          _ <- try env $ addItem $ (Item "59884e90b75c5d1662000000" "Updated" (Tax "5989a7da5d37b91d79c07532" "Tax" 2) 1 2.5 10 (TaxRate "5989a7da5d37b91d79c07551" 2 "TaxRate" 2 "59884e90b75c5d1662000000" Nothing Nothing Nothing) (ItemType "59884e90b75c5d1662000000" "ItemType"))
          items <- try env getItems
          let exists = isJust (find (\x -> itemDescription x == "Updated" && itemCode x == 1) items)
          let count = length $ filter (\x -> itemDescription x == "Updated" && itemCode x == 1) items
          exists && (count == 1) `shouldBe` True

        it "Allows to delete a Item" $ \env -> do
          item <- try env (getItemById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteItem (itemId item)
          try env (getItemById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "Throws a 404 error for missing Items" $ \ env -> do
          try env (getItemById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)
