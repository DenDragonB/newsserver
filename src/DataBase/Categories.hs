{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Categories where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import           Data.Maybe                         (fromMaybe, isNothing,
                                                     listToMaybe)
import           Data.Text                          (Text, pack)
import           GHC.Generics

import           DataBase
import           DataBase.Users
import           Exceptions
import           Logger

data Category = Category
    { id        :: Int
    , name      :: Text
    , parent_id :: Int
    } deriving (Show,Eq,Generic)
instance A.ToJSON Category
instance FromRow Category

categoryAdd ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
categoryAdd param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            catName <- getParamM "name" param

            mpar <-  parseMaybeParam "parent_id" param
            let par = fromMaybe 0 (mpar :: Maybe Int)

            let pool = dbConn env
            isCat <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Categories WHERE CatName = ?);")
                [catName]
            isPar <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Id = ?);")
                [par]
            when (maybe False fromOnly $ listToMaybe isCat) (throwError ObjectExists)
            unless (par == 0 || maybe False fromOnly (listToMaybe isPar)) (throwError ParentNOTExists)
            liftIO $ Logger.debug (Logger.lConfig env) $
                "Try add category name: " <> catName <> "; parent: "<> show par
            _ <- execWithExcept pool
                (Query "INSERT INTO Categories (CatName, Parent) VALUES (?,?);")
                (catName,par)
            liftIO $ Logger.info (Logger.lConfig env) $
                "Add category name: " <> catName
            cat <- queryWithExcept pool
                (Query "SELECT * FROM Categories WHERE CatName = ? ;")
                [catName]
            return $ A.toJSON (cat :: [Category])
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
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            cid <- parseParamM "id" param

            let cname = getMaybeParam "name" param
            par <- parseMaybeParam "parent_id" param

            let pool = dbConn env
            isCat <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Id = ?);")
                [cid :: Int]
            isCatName <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Categories WHERE CatName = ?);")
                [cname]
            isPar <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Id = ?);")
                [par :: Maybe Int]
            unless (maybe False fromOnly $ listToMaybe isCat) (throwError ObjectNOTExists)
            unless (isNothing par || maybe False fromOnly (listToMaybe isPar)) (throwError ParentNOTExists)
            when (maybe False fromOnly $ listToMaybe isCatName) (throwError ObjectExists)

            _ <- execWithExcept pool
                (Query "UPDATE Categories SET "
                    <> "CatName = COALESCE (?, CatName),"
                    <> "Parent = COALESCE (?, Parent )"
                    <> "WHERE Id = ?;")
                (cname,par,cid)
            liftIO $ Logger.info (Logger.lConfig env) $
                "Edit category id: " <> show cid
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
    ) => [( String , Maybe String )]
    -> m A.Value
categoryGet param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (True , _ ) -> do
            let pool = dbConn env
            cid <- parseMaybeParam "id" param
            let cname = getMaybeParam "name" param
            cpar <- parseMaybeParam "parent_id" param
            cat <- queryWithExcept pool
                (Query $ "SELECT id,CatName,Parent FROM Categories WHERE"
                    <> "(id = COALESCE (?, id))"
                    <> "AND (CatName = COALESCE (?, CatName)) "
                    <> "AND (Parent = COALESCE (?, Parent))"
                    <> "ORDER BY Parent,CatName,Id "
                    <> getLimitOffsetBS param <> ";")
                (cid :: Maybe Int, cname, cpar :: Maybe Int)
            return $ A.toJSON (cat :: [Category])
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
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            cid <- parseParamM "id" param

            let pool = dbConn env
            isCat <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Id = ?);")
                [cid :: Int]
            isHaveSub <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Parent = ?);")
                [cid]
            unless (maybe False fromOnly $ listToMaybe isCat) (throwError ObjectNOTExists)
            when (maybe False fromOnly $ listToMaybe isHaveSub) (throwError CategoryWithSub)
            _ <- execWithExcept pool
                (Query "DELETE FROM Categories WHERE Id = ?;")
                [cid]
            liftIO $ Logger.info (Logger.lConfig env) $
                "Delete category id: " <> show cid
            return $ A.String $ "Category with id " <> (pack . show) cid <>" deleted"
        _ -> throwError NotFound
