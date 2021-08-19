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
import           Data.Maybe                         (isNothing, listToMaybe)
import           Data.Text                          (Text, pack, unpack)
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
            mname <- parseParam "name" param
            case mname :: Maybe Text of
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
                        "Add Tag: " <> unpack tagname
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
            tid <- parseParam "id" param
            tname <- parseParam "name" param
            when (isNothing tid || isNothing tname) $ throwError WrongQueryParameter

            let pool = dbConn env
            isTag <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Tags WHERE Id = ? );")
                [tid :: Maybe Int]
            isTagName <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Tags WHERE Tag = ? );")
                [tname :: Maybe Text]
            unless (maybe False fromOnly $ listToMaybe isTag) (throwError ObjectNOTExists)
            when (maybe False fromOnly $ listToMaybe isTagName) (throwError ObjectExists)
            _ <- execWithExcept pool
                (Query "UPDATE Tags SET Tag = ? WHERE Id = ? ;")
                (tname , tid)
            liftIO $ Logger.info (Logger.lConfig env) $
                "Edit Tag id: " <> show tid
            tag <- queryWithExcept pool
                (Query "SELECT * FROM Tags WHERE Id = ? ;")
                [tid]
            return $ A.toJSON (tag :: [Tag])
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
            mtid <- parseParam "id" param
            case mtid :: Maybe Int of
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
                        "Delete Tag id: " <> show tid
                    return $ A.String $ pack $ "Tag with id " <> show tid <> " deleted"
        _ -> throwError NotFound
