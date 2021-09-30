{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Tags where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import           Data.Maybe                         (isNothing)
import           Data.Text                          (Text, pack)
import           GHC.Generics

import           DataBase
import           DataBase.Postgres
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
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            tagname <- getParamM "name" param
            isTag <- existItemByField "Tags" "Tag" tagname
            when isTag (throwError ObjectExists)

            _ <- execWithExcept
                (Query "INSERT INTO Tags (Tag) VALUES (?);")
                [tagname]
            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Add Tag: " <> tagname
            tag <- selectMaybeItemByField "Tags" "Tag" tagname
            return $ A.toJSON (tag :: Maybe Tag)
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
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            tid <- parseMaybeParam "id" param
            when (isNothing tid) $ throwError $ WrongQueryParameter "id"

            let tname = getMaybeParam "name" param
            when (isNothing tname) $ throwError $ WrongQueryParameter "name"

            isTag <- existItemByField "Tags" "Id" (tid :: Maybe Int)
            unless isTag (throwError ObjectNOTExists)
            isTagName <- existItemByField "Tags" "Tag" tname
            when isTagName (throwError ObjectExists)

            _ <- execWithExcept
                (Query "UPDATE Tags SET Tag = ? WHERE Id = ? ;")
                (tname , tid)
            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Edit Tag id: " <> show tid
            tag <- selectMaybeItemByField "Tags" "Id" tid
            return $ A.toJSON (tag :: Maybe Tag)
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
    admin <- findToken param
    case admin of
        Just (True , _ ) -> do
            tid <- parseMaybeParam "id" param
            let tname = getMaybeParam "name" param
            tag <- queryWithExcept
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
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            tid <- parseParamM "id" param

            isTag <- existItemByField "Tags" "Id" (tid :: Maybe Int)
            unless isTag (throwError ObjectNOTExists)

            _ <- execWithExcept
                (Query "DELETE FROM Tags WHERE Id = ? ;")
                [tid]
            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Delete Tag id: " <> show tid
            return $ A.String $ pack $ "Tag with id " <> show tid <> " deleted"
        _ -> throwError NotFound
