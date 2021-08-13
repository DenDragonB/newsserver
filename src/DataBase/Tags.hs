{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Tags where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import qualified Data.ByteString.UTF8               as BS
import           Data.Maybe                         (fromMaybe, listToMaybe)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           GHC.Generics

import           DataBase
import           DataBase.Users
import           Exceptions
import           Logger

data Tag = Tag
    { id   :: Int
    , name :: Text
    } deriving (Show,Eq,Generic)
instance A.ToJSON Tag
instance FromRow Tag

tagAdd ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
tagAdd param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let mname = getParam "name" param
            case mname of
                Nothing -> throwError WrongQueryParameter
                Just name -> do
                    let pool = dbConn env
                    isTag <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Tags WHERE Tag = ?);")
                        [name]
                    when (maybe False fromOnly $ listToMaybe isTag) (throwError ObjectExists)
                    _ <- execWithExcept pool
                        (Query "INSERT INTO Tags (Tag) VALUES (?);")
                        [name]
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Add Tag: " <> BS.toString name
                    tag <- queryWithExcept pool
                        (Query "SELECT * FROM Tags WHERE Tag = ? ;")
                        [name]
                    return $ A.toJSON (tag :: [Tag])
        _ -> throwError NotFound

tagEdit ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
tagEdit param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let mtid = getParam "id" param
            let mname = getParam "name" param
            case sequence [mtid,mname] of
                Just [tid,tname] -> do
                    let pool = dbConn env
                    isTag <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Tags WHERE Id = ? );")
                        [tid]
                    isTagName <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Tags WHERE Tag = ? );")
                        [tname]
                    unless (maybe False fromOnly $ listToMaybe isTag) (throwError ObjectNOTExists)
                    when (maybe False fromOnly $ listToMaybe isTagName) (throwError ObjectExists)
                    _ <- execWithExcept pool
                        (Query "UPDATE Tags SET Tag = ? WHERE Id = ? ;")
                        (tname , tid)
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Edit Tag id: " <> BS.toString tid
                    tag <- queryWithExcept pool
                        (Query "SELECT * FROM Tags WHERE Id = ? ;")
                        [tid]
                    return $ A.toJSON (tag :: [Tag])
                _ -> throwError WrongQueryParameter
        _ -> throwError NotFound

tagGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
tagGet param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (True , _ ) -> do
            let pool = dbConn env
            let tid = getParam "id" param
            let tname = getParam "name" param
            tag <- queryWithExcept pool
                (Query $ "WITH searchData AS (SELECT "
                    <> " CAST (? as INT) AS sid "
                    <> ", CAST (? as TEXT) AS stag ) "
                    <> "SELECT id,tag FROM Tags,searchData WHERE"
                    <> "(stag ISNULL OR stag=tag)"
                    <> "AND (sid ISNULL OR sid=id)"
                    <> "ORDER BY Tag "
                    <> getLimitOffsetBS param <> ";")
                (tid,tname)
            return $ A.toJSON (tag :: [Tag])
        _ -> throwError NotFound

tagDelete ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
tagDelete param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let mtid = getParam "id" param
            case mtid of
                Nothing -> throwError WrongQueryParameter
                Just tid -> do
                    let pool = dbConn env
                    isTag <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Tags WHERE Id = ? );")
                        [tid]
                    unless (maybe False fromOnly $ listToMaybe isTag) (throwError ObjectNOTExists)
                    _ <- execWithExcept pool
                        (Query "DELETE FROM Tags WHERE Id = ? ;")
                        [tid]
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Delete Tag id: " <> BS.toString tid
                    return $ A.String $ decodeUtf8 $ "Tag with id " <> tid <> " deleted"
        _ -> throwError NotFound
