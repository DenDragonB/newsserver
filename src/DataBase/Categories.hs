{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Categories where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import           Data.Maybe                         (fromMaybe, isNothing)
import           Data.Text                          (Text, pack)
import           GHC.Generics

import           DataBase
import           DataBase.Postgres
import           DataBase.Types
import           DataBase.Users
import           Exceptions
import           Logger
import DataBase.Postgres (Tables(Categories), Fields (CatName))

categoryAdd ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
categoryAdd param = do
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            catName <- getParamM "name" param

            mpar <-  parseMaybeParam "parent_id" param
            let par = fromMaybe 0 (mpar :: Maybe Int)


            isCat <- existItemByField Categories CatName catName
            when isCat (throwError ObjectExists)
            isPar <- existItemByField Categories Id par
            unless (par == 0 || isPar) (throwError ParentNOTExists)

            cat <- insertCategoryWithPrameters catName par

            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Add category name: " <> catName

            return $ A.toJSON cat
        _ -> throwError NotFound

categoryEdit ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
categoryEdit param = do
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            cid <- parseParamM "id" param

            let cname = getMaybeParam "name" param
            par <- parseMaybeParam "parent_id" param

            isCat <- existItemByField Categories Id (cid :: Int)
            unless isCat (throwError ObjectNOTExists)
            isCatName <- existItemByField Categories CatName cname
            when isCatName (throwError ObjectExists)
            isPar <- existItemByField Categories Id (par :: Maybe Int)
            unless (isNothing par || isPar) (throwError ParentNOTExists)

            cat <- updateCategoryWithParameters cid par cname

            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Edit category id: " <> show cid

            return $ A.toJSON cat
        _ -> throwError NotFound

categoryGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
categoryGet param = do
    admin <- findToken param
    case admin of
        Just (True , _ ) -> do
            cid <- parseMaybeParam "id" param
            let cname = getMaybeParam "name" param
            cpar <- parseMaybeParam "parent_id" param
            cat <- getCategoryWithParameters cid cpar cname param
            return $ A.toJSON cat
        _ -> throwError NotFound

categoryDelete ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
categoryDelete param = do
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            cid <- parseParamM "id" param
            isCat <- existItemByField Categories Id (cid :: Int)
            unless isCat (throwError ObjectNOTExists)
            isHaveSub <- existItemByField Categories Id cid
            when isHaveSub (throwError CategoryWithSub)

            deleteItemFromTableById Categories cid

            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Delete category id: " <> show cid
            return $ A.String $ "Category with id " <> (pack . show) cid <>" deleted"
        _ -> throwError NotFound
