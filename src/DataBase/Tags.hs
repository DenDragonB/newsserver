{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Tags where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import           Data.Maybe                         (isNothing, listToMaybe)
import           Data.Text                          (Text, pack)
import           Data.Function     
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
    ) => [( String , Maybe String )]
    -> m A.Value
tagAdd param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let mname = getParam "name" param
            tagname <- mname & fromMaybeM WrongQueryParameter

            let pool = dbConn env
            isTag <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Tags WHERE Tag = ?);")
                [tagname]
            when (maybe False fromOnly $ listToMaybe isTag) (throwError ObjectExists)
            _ <- execWithExcept pool
                (Query "INSERT INTO Tags (Tag) VALUES (?);")
                [tagname]
            liftIO $ Logger.info (Logger.lConfig env) $
                "Add Tag: " <> tagname
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
    ) => [( String , Maybe String )]
    -> m A.Value
tagEdit param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            tid <- parseParam "id" param
            when (isNothing tid) $ throwError WrongQueryParameter

            let tname = getParam "name" param
            when (isNothing tname) $ throwError WrongQueryParameter

            let pool = dbConn env
            isTag <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Tags WHERE Id = ? );")
                [tid :: Maybe Int]
            isTagName <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Tags WHERE Tag = ? );")
                [tname]
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
    ) => [( String , Maybe String )]
    -> m A.Value
tagGet param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (True , _ ) -> do
            let pool = dbConn env
            tid <- parseParam "id" param
            let tname = getParam "name" param
            tag <- queryWithExcept pool
                (Query $ "SELECT id,tag FROM Tags WHERE"
                    <> "(id = COALESCE (?, id))"
                    <> "AND (tag = COALESCE (?, tag))"
                    <> "ORDER BY tag "
                    <> getLimitOffsetBS param <> ";")
                (tid :: Maybe Int, tname)
            return $ A.toJSON (tag :: [Tag])
        _ -> throwError NotFound

tagDelete ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
tagDelete param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            mtid <- parseParam "id" param
            tid <- (mtid :: Maybe Int) & fromMaybeM WrongQueryParameter

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
