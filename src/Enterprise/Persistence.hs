{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Enterprise.Persistence
  ( findEnterpriseById
  , findEnterprises
  , insertEnterprise
  , removeEnterprise
  , findEmissionTypeById
  , findEmissionTypes
  , insertEmissionType
  , removeEmissionType
  , findIdentificationTypeById
  , findIdentificationTypes
  , getIdentificationType
  , getIdentificationTypeDoc
  , insertIdentificationType
  , removeIdentificationType
  , findMatrixById
  , findMatrixes
  , insertMatrix
  , removeMatrix
  , findEnvironmentById
  , findEnvironments
  , insertEnvironment
  , removeEnvironment )

where

import qualified Data.Bson as B
import Database.MongoDB
  ( Document
  , delete
  , insert
  , save
  , find
  , findOne
  , rest
  , select
  , (=:) )

import Common.Types
import Common.Persistence
import Enterprise.Types

findEnterpriseById :: String -> App (Maybe Enterprise)
findEnterpriseById _id = do
  enterpriseDoc <- runDb $ findOne (select ["_id" =: (B.ObjId $ read _id)] "enterprises")
  enterprise <- pure $ getEnterprise enterpriseDoc
  return enterprise

findEnterprises :: App [Maybe Enterprise]
findEnterprises = do
  enterpriseDocs <- runDb $ rest =<< find (select [] "enterprises")
  enterprises <- pure $ map getEnterprise (Just <$> enterpriseDocs)
  return enterprises

getEnterprise :: Maybe Document -> Maybe Enterprise
getEnterprise document =
  case document of
     Nothing   -> Nothing
     Just doc  -> Enterprise
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "name" doc
                     <*> B.lookup "identification" doc
                     <*> getIdentificationType (B.lookup "identification_type" doc)
                     <*> getEmissionType (B.lookup "emission_type" doc)
                     <*> getEnvironment (B.lookup "environment" doc)
                     <*> B.lookup "p12_url" doc
                     <*> B.lookup "address" doc
                     <*> B.lookup "telephone" doc
                     <*> B.lookup "email" doc
                     <*> B.lookup "special_contributor" doc
                     <*> B.lookup "accounting_obliged" doc
                     <*> mapM getMatrix (B.lookup "matrixes" doc)

-- this code is not secure
-- what happens if the insert failed?
-- Maybe pattern match with Failure type
insertEnterprise :: Enterprise -> App Enterprise
insertEnterprise Enterprise{..} =
  case entId of
    "" -> do
      _id <- runDb $
        insert "enterprises" [ "name" =: entName
                             , "identification" =: entIdentification
                             , "identification_type" =: [ "_id" =: (B.ObjId $ read (itId entIdentificationType))
                                                        , "description" =: itDescription entIdentificationType
                                                        , "code" =: itCode entIdentificationType ]
                             , "emission_type" =: [ "_id" =: (B.ObjId $ read (etId entEmissionType))
                                                  , "description" =: etDescription entEmissionType
                                                  , "code" =: etCode entEmissionType]
                             , "environment" =: [ "_id" =: (B.ObjId $ read (envId entEnvironment))
                                                , "description" =: envDescription entEnvironment
                                                , "code" =: envCode entEnvironment ]
                             , "p12_url" =: entP12Url
                             , "address" =: entAddress
                             , "telephone" =: entTelephone
                             , "email" =: entEmail
                             , "special_contributor" =: entSpecialContributor
                             , "accounting_obliged" =: entAccountingObliged
                             , "matrixes" =: map getMatrixDoc entMatrixes ]

      return $ Enterprise (show _id) entName entIdentification entIdentificationType entEmissionType entEnvironment entP12Url entAddress entTelephone entEmail entSpecialContributor entAccountingObliged entMatrixes
    _ -> do
      runDb $
        save "enterprises" [ "_id" =: B.ObjId (read entId)
                           , "name" =: entName
                           , "identification" =: entIdentification
                           , "identification_type" =: [ "_id" =: (B.ObjId $ read (itId entIdentificationType))
                                                      , "description" =: itDescription entIdentificationType
                                                      , "code" =: itCode entIdentificationType ]
                           , "emission_type" =: [ "_id" =: (B.ObjId $ read (etId entEmissionType))
                                                , "description" =: etDescription entEmissionType
                                                , "code" =: etCode entEmissionType]
                           , "environment" =: [ "_id" =: (B.ObjId $ read (envId entEnvironment))
                                              , "description" =: envDescription entEnvironment
                                              , "code" =: envCode entEnvironment ]
                           , "p12_url" =: entP12Url
                           , "address" =: entAddress
                           , "telephone" =: entTelephone
                           , "email" =: entEmail
                           , "special_contributor" =: entSpecialContributor
                           , "accounting_obliged" =: entAccountingObliged
                           , "matrixes" =: map getMatrixDoc entMatrixes ]
      return Enterprise{..}

removeEnterprise :: String -> App ()
removeEnterprise _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "enterprises"

findIdentificationTypeById :: String -> App (Maybe IdentificationType)
findIdentificationTypeById _id = do
  itDoc <- runDb $ findOne (select ["_id" =: B.ObjId (read _id)] "identification_types")
  identificationType <- pure $ getIdentificationType itDoc
  return identificationType

findIdentificationTypes :: App [Maybe IdentificationType]
findIdentificationTypes = do
  itDocs <- runDb $ rest =<< find (select [] "identification_types")
  identificationTypes <- pure $ map getIdentificationType (Just <$> itDocs)
  return identificationTypes

getIdentificationType :: Maybe Document -> Maybe IdentificationType
getIdentificationType document =
  case document of
     Nothing   -> Nothing
     Just doc  -> IdentificationType
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "description" doc
                     <*> B.lookup "code"        doc

getIdentificationTypeDoc :: IdentificationType -> Document
getIdentificationTypeDoc IdentificationType{..} = [ "_id" =: B.ObjId (read itId)
                                                  , "description" =: itDescription
                                                  , "code" =: itCode ]

insertIdentificationType :: IdentificationType -> App IdentificationType
insertIdentificationType IdentificationType{..} =
  case itId of
    "" -> do
      _id <- runDb $
        insert "identification_types" ["code" =: itCode, "description" =: itDescription]
      return $ IdentificationType (show _id) itDescription itCode
    _ -> do
      runDb $
        save "identification_types" ["_id" =: B.ObjId (read itId), "code" =: itCode, "description" =: itDescription]
      return $ IdentificationType itId itDescription itCode

removeIdentificationType :: String -> App ()
removeIdentificationType _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "identification_types"

findEmissionTypeById :: String -> App (Maybe EmissionType)
findEmissionTypeById _id = do
  etDoc <- runDb $ findOne (select ["_id" =: B.ObjId (read _id)] "emission_types")
  emissionType <- pure $ getEmissionType etDoc
  return emissionType

findEmissionTypes :: App [Maybe EmissionType]
findEmissionTypes = do
  etDocs <- runDb $ rest =<< find (select [] "emission_types")
  emissionTypes <- pure $ map getEmissionType (Just <$> etDocs)
  return emissionTypes

getEmissionType :: Maybe Document -> Maybe EmissionType
getEmissionType document =
  case document of
     Nothing   -> Nothing
     Just doc  -> EmissionType
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "description" doc
                     <*> B.lookup "code"        doc

insertEmissionType :: EmissionType -> App EmissionType
insertEmissionType EmissionType{..} =
  case etId of
    "" -> do
      _id <- runDb $
        insert "emission_types" ["code" =: etCode, "description" =: etDescription]
      return $ EmissionType (show _id) etDescription etCode
    _ -> do
      runDb $
        save "emission_types" ["_id" =: B.ObjId (read etId), "code" =: etCode, "description" =: etDescription]
      return $ EmissionType etId etDescription etCode

removeEmissionType :: String -> App ()
removeEmissionType _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "emission_types"

findMatrixById :: String -> App (Maybe Matrix)
findMatrixById _id = do
  matrixDoc <- runDb $ findOne (select ["_id" =: B.ObjId (read _id)] "matrixes")
  matrix <- pure $ getMatrix matrixDoc
  return matrix

findMatrixes :: App [Maybe Matrix]
findMatrixes = do
  matrixDocs <- runDb $ rest =<< find (select [] "matrixes")
  matrixes <- pure $ map getMatrix (Just <$> matrixDocs)
  return matrixes

getMatrix :: Maybe Document -> Maybe Matrix
getMatrix document =
  case document of
     Nothing   -> Nothing
     Just doc  -> Matrix
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "description" doc
                     <*> B.lookup "number"        doc

getMatrixDoc :: Matrix -> Document
getMatrixDoc Matrix{..} = [ "_id" =: B.ObjId (read matrixId)
                          , "description" =: matrixDescription
                          , "number" =: matrixNumber ]

insertMatrix :: Matrix -> App Matrix
insertMatrix Matrix{..} =
  case matrixId of
    "" -> do
      _id <- runDb $
        insert "matrixes" ["number" =: matrixNumber, "description" =: matrixDescription]
      return $ Matrix (show _id) matrixDescription matrixNumber
    _ -> do
      runDb $
        save "matrixes" ["_id" =: B.ObjId (read matrixId), "number" =: matrixNumber, "description" =: matrixDescription]
      return $ Matrix matrixId matrixDescription matrixNumber

removeMatrix :: String -> App ()
removeMatrix _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "matrixes"

findEnvironmentById :: String -> App (Maybe Environment)
findEnvironmentById _id = do
  envDoc <- runDb $ findOne (select ["_id" =: B.ObjId (read _id)] "environments")
  env <- pure $ getEnvironment envDoc
  return env

findEnvironments :: App [Maybe Environment]
findEnvironments = do
  envDocs <- runDb $ rest =<< find (select [] "environments")
  environments <- pure $ map getEnvironment (Just <$> envDocs)
  return environments

getEnvironment :: Maybe Document -> Maybe Environment
getEnvironment document =
  case document of
     Nothing   -> Nothing
     Just doc  -> Environment
                     <$> show <$> (B.lookup "_id" doc :: Maybe B.ObjectId)
                     <*> B.lookup "description" doc
                     <*> B.lookup "code"        doc

insertEnvironment :: Environment -> App Environment
insertEnvironment Environment{..} =
  case envId of
    "" -> do
      _id <- runDb $
        insert "environments" ["code" =: envCode, "description" =: envDescription]
      return $ Environment (show _id) envDescription envCode
    _ -> do
      runDb $
        save "environments" ["_id" =: B.ObjId (read envId), "code" =: envCode, "description" =: envDescription]
      return $ Environment envId envDescription envCode

removeEnvironment :: String -> App ()
removeEnvironment _id = runDb $ delete $ select ["_id" =: B.ObjId (read _id)] "environments"
