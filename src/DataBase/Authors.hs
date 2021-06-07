{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module DataBase.Authors where

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

data Author = Author
    { aid :: Int 
    , userId :: Int
    , about :: Text
    } deriving (Show,Eq)
instance A.ToJSON Author where
    toJSON author = A.object 
        [ "id"         A..= aid author
        , "user_id"    A..= userId author
        , "about"      A..= about author
        ]    
instance FromRow Author where
    fromRow = Author 
        <$> field
        <*> field 
        <*> field

authorAdd :: 
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
authorAdd param = do
    env <- ask
    admin <- findToken param
    liftIO $ Logger.debug (Logger.lConfig env) $ 
        "Just (User,Admin): " <> show admin
    case admin of
        Just (_, True ) -> do
            let muid = getParam "user_id" param
            let mabout = getParam "about" param
            case sequence [muid,mabout] of
                Nothing -> throwError WrongQueryParameter
                Just [uid,about] -> do
                    let pool = dbConn env
                    isUser <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Users WHERE id = " <> uid <> ");"
                    unless (fromOnly $ head isUser) (throwError UserNOTExists)   
                    isAuthor <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Authors WHERE UserId = " <> uid <> ");"
                    when (fromOnly $ head isAuthor) (throwError ObjectExists)
                    _ <- liftIO $ execDB pool $ Query $
                        "INSERT INTO Authors (UserId, About) VALUES" 
                        <> "(" <> uid <> ",'" <> about <> "');"
                    liftIO $ Logger.info (Logger.lConfig env) $ 
                        "Add author with user id: " <> BS.toString uid
                    author <- liftIO $ queryDB pool $ Query $
                        "SELECT * FROM Authors WHERE UserId = " <> uid <> ";"
                    return $ A.toJSON (author :: [Author])
        _ -> throwError NotFound

authorEdit :: 
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
authorEdit param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let uid = fromMaybe "" $ getParam "user_id" param
            let about = fromMaybe "" $ getParam "about" param
            let maid = getParam "id" param
            case maid of
                Nothing -> throwError WrongQueryParameter
                Just aid -> do
                    let pool = dbConn env
                    isAuthor <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Authors WHERE Id = " <> aid <> ");"
                    unless (fromOnly $ head isAuthor) (throwError ObjectNOTExists)
                    _ <- liftIO $ execDB pool $ Query $
                        "UPDATE Authors SET Id = " <> aid
                        <> addToUpdate "UserId" uid
                        <> addToUpdate "About" about 
                        <> " WHERE Id = " <> aid <> ";"
                    liftIO $ Logger.info (Logger.lConfig env) $ 
                        "Edit author id: " <> BS.toString aid
                    author <- liftIO $ queryDB pool $ Query $
                        "SELECT * FROM Authors WHERE Id = " <> aid <> ";"
                    return $ A.toJSON (author :: [Author])
        _ -> throwError NotFound

authorGet :: 
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
authorGet param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let pool = dbConn env
            let uid = fromMaybe "" $ getParam "user_id" param
            let aid = fromMaybe "" $ getParam "id" param
            author <- liftIO $ queryDB pool $ Query $
                "SELECT * FROM Authors WHERE Id > 0 "
                <> addFieldToQueryNumBS "Id" aid 
                <> addFieldToQueryNumBS "UserId" uid
                <> getLimitOffsetBS param
                <> ";"
            return $ A.toJSON (author :: [Author])
        _ -> throwError NotFound

authorDelete :: 
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
authorDelete param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let maid = getParam "id" param
            case maid of
                Nothing -> throwError WrongQueryParameter
                Just aid -> do
                    let pool = dbConn env
                    isAuthor <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Authors WHERE Id = " <> aid <> ");"
                    unless (fromOnly $ head isAuthor) (throwError ObjectNOTExists)
                    _ <- liftIO $ execDB pool $ Query $
                        "DELETE FROM Authors WHERE Id = "<> aid <> ";"
                    liftIO $ Logger.info (Logger.lConfig env) $ 
                        "Delete author id: " <> BS.toString aid
                    return $ A.String $ decodeUtf8 $ "Author with id "<>aid<>" deleted"
        _ -> throwError NotFound