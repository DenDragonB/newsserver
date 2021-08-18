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
import           Data.Maybe                         (listToMaybe)
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
                Just tagname -> do
                    let pool = dbConn env
                    isTag <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Tags WHERE Tag = ?);")
                        [tagname]
                    when (maybe False fromOnly $ listToMaybe isTag) (throwError ObjectExists)
                    _ <- execWithExcept pool
                        (Query "INSERT INTO Tags (Tag) VALUES (?);")
                        [tagname]
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Add Tag: " <> BS.toString tagname
                    tag <- queryWithExcept pool
                        (Query "SELECT * FROM Tags WHERE Tag = ? ;")
                        [tagname]
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
            tid <- parseParam "id" param 
            tname <- parseParam "name" param
            tag <- queryWithExcept pool
                (Query $ "SELECT id,tag FROM Tags WHERE"
                    <> "(id = COALESCE (?, id))"
                    <> "AND (tag = COALESCE (?, tag))"
                    <> "ORDER BY tag "
                    <> getLimitOffsetBS param <> ";")
                (tid :: Maybe Int,tname :: Maybe Text)
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
