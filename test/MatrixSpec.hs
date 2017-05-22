{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module MatrixSpec where

import Data.Maybe (isJust)
import Data.List (find, filter)
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec

import Lib
import Enterprise.API hiding (getMatrixes, getMatrixById, addMatrix, deleteMatrix)
import Common.Types
import Enterprise.Types
import Test

getMatrixes :: ClientM [Matrix]
getMatrixById :: String -> ClientM Matrix
addMatrix :: Matrix -> ClientM Matrix
deleteMatrix :: String -> ClientM NoContent

getMatrixes :<|> getMatrixById :<|> addMatrix :<|> deleteMatrix = client matrixApi

spec :: Spec
spec = beforeAll_ flushDB $
    describe "/matrix" $
      withClient (mkApp (AppEnv (DbConfig "127.0.0.1" "dev" "root" "dev") "logs")) $ do
        it "Allows to create a new Matrix" $ \ env -> do
          _ <- try env $ addMatrix (Matrix "59884e90b75c5d1662000000" "New One" 3)
          matrixes <- try env getMatrixes
          isJust (find (\x -> matrixDescription x == "New One") matrixes) `shouldBe` True

        it "Allows to show Matrixes by id" $ \ env -> do
          matrix <- try env (getMatrixById "59884e90b75c5d1662000000")
          matrixDescription matrix `shouldBe` "New One"

        it "Allows to update an Matrix" $ \env -> do
          matrix <- try env (getMatrixById "59884e90b75c5d1662000000")
          _ <- try env $ addMatrix $ Matrix (matrixId matrix) "Updated" (matrixNumber matrix)
          matrixs <- try env getMatrixes
          let exists = isJust (find (\x -> matrixDescription x == "Updated" && matrixNumber x == 3) matrixs)
          let count = length $ filter (\x -> matrixDescription x == "Updated" && matrixNumber x == 3) matrixs
          exists && (count == 1) `shouldBe` True

        it "Allows to delete an Matrix" $ \env -> do
          matrix <- try env (getMatrixById "59884e90b75c5d1662000000")
          _ <- liftIO $ try env $ deleteMatrix (matrixId matrix)
          try env (getMatrixById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)

        it "Throws a 404 error for missing Matrixes" $ \ env -> do
          try env (getMatrixById "59884e90b75c5d1662000000") `shouldThrow` (\e -> responseStatus e == notFound404)
