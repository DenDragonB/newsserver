{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module DataBase.Categories where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types


import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Aeson as A
import qualified Data.ByteString.UTF8 as BS
import           Data.Text ( Text )
import           Data.Text.Encoding (decodeUtf8)
import           Data.Maybe (fromMaybe)

import           DataBase
import           DataBase.Users
import           Exceptions
import           Logger

data Category = Category
    { cid :: Int 
    , catName :: Text
    , parent :: Int
    } deriving (Show,Eq)
instance A.ToJSON Category where
    toJSON cat = A.object 
        [ "id"        A..= cid cat
        , "name"      A..= catName cat
        , "parent_id" A..= parent cat
        ]    
instance FromRow Category where
    fromRow = Category
        <$> field
        <*> field 
        <*> field

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
                    isCat <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Categories WHERE CatName = '" <> name <> "');"
                    isPar <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Categories WHERE Id = " <> par <> ");"    
                    when (fromOnly $ head isCat) (throwError ObjectExists)
                    unless (par == "0" || fromOnly (head isPar)) (throwError ParentNOTExists)
                    liftIO $ Logger.debug (Logger.lConfig env) $ 
                        "Try add category name: " <> BS.toString name <> "; parent: "<> BS.toString par
                    _ <- liftIO $ execDB pool $ Query $
                        "INSERT INTO Categories (CatName, Parent) VALUES" 
                        <> "('" <> name <> "'," <> par <> ");"
                    liftIO $ Logger.info (Logger.lConfig env) $ 
                        "Add category name: " <> BS.toString name
                    cat <- liftIO $ queryDB pool $ Query $
                        "SELECT * FROM Categories WHERE CatName = '" <> name <> "';"
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
            let cname = fromMaybe "" $ getParam "name" param
            let par = fromMaybe "0" $ getParam "parent_id" param
            case mcid of
                Nothing -> throwError WrongQueryParameter
                Just cid -> do
                    let pool = dbConn env
                    isCat <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Categories WHERE Id = " <> cid <> ");"
                    isCatName <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Categories WHERE CatName = '" <> cname <> "');"
                    isPar <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Categories WHERE Id = " <> par <> ");"    
                    unless (fromOnly $ head isCat) (throwError ObjectNOTExists)
                    unless (par == "0" || fromOnly (head isPar)) (throwError ParentNOTExists)
                    when (fromOnly $ head isCatName) (throwError ObjectExists)
                    _ <- liftIO $ execDB pool $ Query $
                        "UPDATE Categories SET Id = " <> cid
                        <> addToUpdate "CatName" cname
                        <> addToUpdateNum "Parent" par 
                        <> " WHERE Id = " <> cid <> ";"
                    liftIO $ Logger.info (Logger.lConfig env) $ 
                        "Edit category id: " <> BS.toString cid
                    cat <- liftIO $ queryDB pool $ Query $
                        "SELECT * FROM Categories WHERE Id = " <> cid <> ";"
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
            let cid = fromMaybe "" $ getParam "id" param
            let cname = fromMaybe "" $ getParam "name" param
            let cpar = fromMaybe "" $ getParam "parent_id" param
            cat <- liftIO $ queryDB pool $ Query $
                "SELECT * FROM Categories WHERE Id > 0 "
                <> addFieldToQueryNumBS "Id" cid 
                <> addFieldToQueryBS "CatName" cname
                <> addFieldToQueryNumBS "Parent" cpar 
                <> getLimitOffsetBS param
                <> ";"
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
                    isCat <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Categories WHERE Id = " <> cid <> ");"
                    isHaveSub <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Categories WHERE Parent = " <> cid <> ");"
                    unless (fromOnly $ head isCat) (throwError ObjectNOTExists)
                    when (fromOnly $ head isHaveSub) (throwError CategoryWithSub)
                    _ <- liftIO $ execDB pool $ Query $
                        "DELETE FROM Categories WHERE Id = "<> cid <> ";"
                    liftIO $ Logger.info (Logger.lConfig env) $ 
                        "Delete category id: " <> BS.toString cid
                    return $ A.String $ decodeUtf8 $ "Category with id "<>cid<>" deleted"
        _ -> throwError NotFound