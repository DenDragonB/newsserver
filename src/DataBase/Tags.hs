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
import           DataBase.Types
import           DataBase.Users
import           Exceptions
import           Logger

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
            isTag <- existItemByField Tags Tag tagname
            when isTag (throwError ObjectExists)

            tag <- insertTagWitnName tagname
            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Add Tag: " <> tagname
            return $ A.toJSON tag
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
            tid <- parseParamM "id" param
            tname <- getParamM "name" param

            isTag <- existItemByField Tags Id tid
            unless isTag (throwError ObjectNOTExists)
            isTagName <- existItemByField Tags Tag tname
            when isTagName (throwError ObjectExists)

            tag <- updateTagById tid tname

            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Edit Tag id: " <> show tid
            return $ A.toJSON tag
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
            tag <- selectTagsWithParameters tid tname param
            return $ A.toJSON tag
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

            isTag <- existItemByField Tags Id tid
            unless isTag (throwError ObjectNOTExists)

            deleteItemFromTableById Tags tid

            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Delete Tag id: " <> show tid
            return $ A.String $ pack $ "Tag with id " <> show tid <> " deleted"
        _ -> throwError NotFound
