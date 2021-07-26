{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Categories where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import qualified Data.ByteString.UTF8               as BS
import           Data.Maybe                         (fromMaybe, isNothing,
                                                     listToMaybe)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           GHC.Generics

import           DataBase
import           DataBase.Users
import           Exceptions
import           Logger

data Category = Category
    { cid     :: Int
    , catName :: Text
    , parent  :: Int
    } deriving (Show,Eq,Generic)
instance A.ToJSON Category where
    toJSON cat = A.object
        [ "id"        A..= cid cat
        , "name"      A..= catName cat
        , "parent_id" A..= parent cat
        ]
instance FromRow Category

categoryAdd ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
categoryAdd param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let mname = getParam "name" param
            let par = fromMaybe "0" $ getParam "parent_id" param
            case mname of
                Nothing -> throwError WrongQueryParameter
                Just name -> do
                    let pool = dbConn env
                    isCat <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Categories WHERE CatName = ?);")
                        [name]
                    isPar <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Id = ?);")
                        [par]
                    when (maybe False fromOnly $ listToMaybe isCat) (throwError ObjectExists)
                    unless (par == "0" || maybe False fromOnly (listToMaybe isPar)) (throwError ParentNOTExists)
                    liftIO $ Logger.debug (Logger.lConfig env) $
                        "Try add category name: " <> BS.toString name <> "; parent: "<> BS.toString par
                    _ <- execWithExcept pool
                        (Query "INSERT INTO Categories (CatName, Parent) VALUES (?,?);")
                        (name,par)
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Add category name: " <> BS.toString name
                    cat <- queryWithExcept pool
                        (Query "SELECT * FROM Categories WHERE CatName = ? ;")
                        [name]
                    return $ A.toJSON (cat :: [Category])
        _ -> throwError NotFound

categoryEdit ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
categoryEdit param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let mcid = getParam "id" param
            let cname = getParam "name" param
            let par = getParam "parent_id" param
            case mcid of
                Nothing -> throwError WrongQueryParameter
                Just cid -> do
                    let pool = dbConn env
                    isCat <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Id = ?);")
                        [cid]
                    isCatName <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Categories WHERE CatName = ?);")
                        [cname]
                    isPar <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Id = ?);")
                        [par]
                    unless (maybe False fromOnly $ listToMaybe isCat) (throwError ObjectNOTExists)
                    unless (isNothing par || maybe False fromOnly (listToMaybe isPar)) (throwError ParentNOTExists)
                    when (maybe False fromOnly $ listToMaybe isCatName) (throwError ObjectExists)

                    _ <- execWithExcept pool
                        (Query "WITH "
                            <> "newData AS (SELECT CAST (? AS TEXT) as CatName, CAST (? AS INT) as Parent),"
                            <> "oldData AS (SELECT id,CatName,Parent from Categories WHERE id = ?)"
                            <> "UPDATE Categories SET "
                            <> "CatName = COALESCE ((SELECT CatName from newData),(SELECT CatName from oldData)),"
                            <> "Parent = COALESCE ((SELECT Parent from newData),(SELECT Parent from oldData))"
                            <> "WHERE Id = (SELECT id from oldData);")
                        (cname,par,cid)
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Edit category id: " <> BS.toString cid
                    cat <- queryWithExcept pool
                        (Query "SELECT * FROM Categories WHERE Id = ?;")
                        [cid]
                    return $ A.toJSON (cat :: [Category])
        _ -> throwError NotFound

categoryGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
categoryGet param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (True , _ ) -> do
            let pool = dbConn env
            let cid = getParam "id" param
            let cname = getParam "name" param
            let cpar = getParam "parent_id" param
            cat <- queryWithExcept pool
                (Query $ "WITH searchData AS (SELECT "
                    <> " CAST (? as INT) AS sid "
                    <> ", CAST (? as TEXT) AS sCatName "
                    <> ", CAST (? as INT) AS sParent ) "
                    <> "SELECT id,CatName,Parent FROM Categories,searchData WHERE"
                    <> "(sid ISNULL OR sid=id)"
                    <> "AND (sCatName ISNULL OR sCatName=CatName) "
                    <> "AND (sParent ISNULL OR sParent=Parent)"
                    <> "ORDER BY Parent,CatName,Id "
                    <> getLimitOffsetBS param <> ";")
                (cid,cname,cpar)
            return $ A.toJSON (cat :: [Category])
        _ -> throwError NotFound

categoryDelete ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
categoryDelete param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let mcid = getParam "id" param
            case mcid of
                Nothing -> throwError WrongQueryParameter
                Just cid -> do
                    let pool = dbConn env
                    isCat <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Id = ?);")
                        [cid]
                    isHaveSub <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Parent = ?);")
                        [cid]
                    unless (maybe False fromOnly $ listToMaybe isCat) (throwError ObjectNOTExists)
                    when (maybe False fromOnly $ listToMaybe isHaveSub) (throwError CategoryWithSub)
                    _ <- execWithExcept pool
                        (Query "DELETE FROM Categories WHERE Id = ?;")
                        [cid]
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Delete category id: " <> BS.toString cid
                    return $ A.String $ decodeUtf8 $ "Category with id "<>cid<>" deleted"
        _ -> throwError NotFound
