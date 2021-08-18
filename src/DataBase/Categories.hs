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
import           Data.Text                          (Text,unpack,pack)
import           Data.Text.Encoding                 (decodeUtf8)
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
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
categoryAdd param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            mname <- parseParam "name" param
            mpar <-  parseParam "parent_id" param
            let par = fromMaybe 0 (mpar :: Maybe Int)
            case mname :: Maybe Text of
                Nothing -> throwError WrongQueryParameter
                Just catName -> do
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
                        "Try add category name: " <> unpack catName <> "; parent: "<> show par
                    _ <- execWithExcept pool
                        (Query "INSERT INTO Categories (CatName, Parent) VALUES (?,?);")
                        (catName,par)
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Add category name: " <> unpack catName
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
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
categoryEdit param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            mcid <- parseParam "id" param
            cname <- parseParam "name" param
            par <- parseParam "parent_id" param
            case mcid :: Maybe Int of
                Nothing -> throwError WrongQueryParameter
                Just cid -> do
                    let pool = dbConn env
                    isCat <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Categories WHERE Id = ?);")
                        [cid]
                    isCatName <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Categories WHERE CatName = ?);")
                        [cname :: Maybe Text]
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
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
categoryGet param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (True , _ ) -> do
            let pool = dbConn env
            cid <- parseParam "id" param
            cname <- parseParam "name" param
            cpar <- parseParam "parent_id" param
            cat <- queryWithExcept pool
                (Query $ "SELECT id,CatName,Parent FROM Categories WHERE"
                    <> "(id = COALESCE (?, id))"
                    <> "AND (CatName = COALESCE (?, CatName)) "
                    <> "AND (Parent = COALESCE (?, Parent))"
                    <> "ORDER BY Parent,CatName,Id "
                    <> getLimitOffsetBS param <> ";")
                (cid :: Maybe Int, cname :: Maybe Text, cpar :: Maybe Int)
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
            mcid <- parseParam "id" param
            case mcid :: Maybe Int of
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
                        "Delete category id: " <> show cid
                    return $ A.String $ "Category with id " <> (pack . show) cid<>" deleted"
        _ -> throwError NotFound
