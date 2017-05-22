{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Enterprise.API where

import Control.Monad.Trans.Except
import Control.Monad.Reader
import Data.Maybe
import Servant

import Common.Types
import Enterprise.Types
import Enterprise.Persistence

type EnterpriseApi =
  "enterprises" :> Get '[JSON] [Enterprise] :<|>
    "enterprise" :> Capture "enterpriseId" String :> Get '[JSON] Enterprise :<|>
      "enterprise" :> ReqBody '[JSON] Enterprise :> Post '[JSON] Enterprise :<|>
        "enterprise" :> Capture "enterpriseId" String :> DeleteNoContent '[JSON] NoContent

type IdentificationTypeApi =
  "identification-types" :> Get '[JSON] [IdentificationType] :<|>
    "identification-type" :> Capture "identificationTypeId" String :> Get '[JSON] IdentificationType :<|>
      "identification-type" :> ReqBody '[JSON] IdentificationType :> Post '[JSON] IdentificationType :<|>
        "identification-type" :> Capture "identificationTypeId" String :> DeleteNoContent '[JSON] NoContent

type EmissionTypeApi =
  "emission-types" :> Get '[JSON] [EmissionType] :<|>
    "emission-type" :> Capture "emissionTypeId" String :> Get '[JSON] EmissionType :<|>
      "emission-type" :> ReqBody '[JSON] EmissionType :> Post '[JSON] EmissionType :<|>
        "emission-type" :> Capture "emissionTypeId" String :> DeleteNoContent '[JSON] NoContent

type MatrixApi =
  "matrixes" :> Get '[JSON] [Matrix] :<|>
    "matrix" :> Capture "matrixId" String :> Get '[JSON] Matrix :<|>
      "matrix" :> ReqBody '[JSON] Matrix :> Post '[JSON] Matrix :<|>
        "matrix" :> Capture "matrixId" String :> DeleteNoContent '[JSON] NoContent

type EnvironmentApi =
  "environments" :> Get '[JSON] [Environment] :<|>
    "environment" :> Capture "environmentId" String :> Get '[JSON] Environment :<|>
      "environment" :> ReqBody '[JSON] Environment :> Post '[JSON] Environment :<|>
        "environment" :> Capture "environmentId" String :> DeleteNoContent '[JSON] NoContent

type FullEnterpriseApi = EnterpriseApi
                    :<|> IdentificationTypeApi
                    :<|> EmissionTypeApi
                    :<|> MatrixApi
                    :<|> EnvironmentApi

fullEnterpriseServer :: AppEnv -> Server FullEnterpriseApi
fullEnterpriseServer env = enterpriseServer env
                      :<|> identificationTypeServer env
                      :<|> emissionTypeServer env
                      :<|> matrixServer env
                      :<|> environmentServer env

enterpriseApi :: Proxy EnterpriseApi
enterpriseApi = Proxy

enterpriseServer :: AppEnv -> Server EnterpriseApi
enterpriseServer env =
  enter (runReaderTNat env) $ getEnterprises :<|> getEnterpriseById :<|> addEnterprise :<|> deleteEnterprise

getEnterprises :: App [Enterprise]
getEnterprises = do
  enterprises <- findEnterprises
  return $ catMaybes enterprises

getEnterpriseById :: String -> App Enterprise
getEnterpriseById _id = do
  enterprise <- findEnterpriseById _id
  case enterprise of
    Nothing -> (lift . throwE) err404
    Just enterprise' -> return enterprise'

addEnterprise :: Enterprise -> App Enterprise
addEnterprise = insertEnterprise

deleteEnterprise :: String -> App NoContent
deleteEnterprise _id = do
  removeEnterprise _id
  return NoContent

identificationTypeApi :: Proxy IdentificationTypeApi
identificationTypeApi = Proxy

identificationTypeServer :: AppEnv -> Server IdentificationTypeApi
identificationTypeServer env =
  enter (runReaderTNat env) $ getIdentificationTypes :<|> getIdentificationTypeById :<|> addIdentificationType :<|> deleteIdentificationType

getIdentificationTypes :: App [IdentificationType]
getIdentificationTypes = do
  identificationTypes <- findIdentificationTypes
  return $ catMaybes identificationTypes

getIdentificationTypeById :: String -> App IdentificationType
getIdentificationTypeById _id = do
  identificationType <- findIdentificationTypeById _id
  case identificationType of
    Nothing -> (lift . throwE) err404
    Just identificationType' -> return identificationType'

addIdentificationType :: IdentificationType -> App IdentificationType
addIdentificationType = insertIdentificationType

deleteIdentificationType :: String -> App NoContent
deleteIdentificationType _id = do
  removeIdentificationType _id
  return NoContent

emissionTypeApi :: Proxy EmissionTypeApi
emissionTypeApi = Proxy

emissionTypeServer :: AppEnv -> Server EmissionTypeApi
emissionTypeServer env =
  enter (runReaderTNat env) $ getEmissionTypes:<|> getEmissionTypeById :<|> addEmissionType:<|> deleteEmissionType

getEmissionTypes :: App [EmissionType]
getEmissionTypes = do
  emissionTypes <- findEmissionTypes
  return $ catMaybes emissionTypes

getEmissionTypeById :: String -> App EmissionType
getEmissionTypeById _id = do
  emissionType <- findEmissionTypeById _id
  case emissionType of
    Nothing -> (lift . throwE) err404
    Just emissionType' -> return emissionType'

addEmissionType :: EmissionType -> App EmissionType
addEmissionType = insertEmissionType

deleteEmissionType :: String -> App NoContent
deleteEmissionType _id = do
  removeEmissionType _id
  return NoContent

matrixApi :: Proxy MatrixApi
matrixApi = Proxy

matrixServer :: AppEnv -> Server MatrixApi
matrixServer env =
  enter (runReaderTNat env) $ getMatrixes :<|> getMatrixById :<|> addMatrix :<|> deleteMatrix

getMatrixes :: App [Matrix]
getMatrixes = do
  matrixes <- findMatrixes
  return $ catMaybes matrixes

getMatrixById :: String -> App Matrix
getMatrixById _id = do
  matrix <- findMatrixById _id
  case matrix of
    Nothing -> (lift . throwE) err404
    Just matrix' -> return matrix'

addMatrix :: Matrix -> App Matrix
addMatrix = insertMatrix

deleteMatrix :: String -> App NoContent
deleteMatrix _id = do
  removeMatrix _id
  return NoContent

environmentApi :: Proxy EnvironmentApi
environmentApi = Proxy

environmentServer :: AppEnv -> Server EnvironmentApi
environmentServer env =
  enter (runReaderTNat env) $
    getEnvironments :<|>
      getEnvironmentById :<|>
        addEnvironment :<|>
          deleteEnvironment

getEnvironmentById :: String -> App Environment
getEnvironmentById _id = do
  environment <- findEnvironmentById _id
  case environment of
    Nothing -> (lift . throwE) err404
    Just env -> return env

getEnvironments :: App [Environment]
getEnvironments = do
  envs <- findEnvironments
  return $ catMaybes envs

addEnvironment :: Environment -> App Environment
addEnvironment = insertEnvironment

deleteEnvironment :: String -> App NoContent
deleteEnvironment _id = do
  removeEnvironment _id
  return NoContent
