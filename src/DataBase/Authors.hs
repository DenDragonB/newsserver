{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module DataBase.Authors where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types


import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Aeson as A
import qualified Data.ByteString.UTF8 as BS
import           Data.Text ( Text )
import           Data.Time
import           Data.Maybe
import           Crypto.BCrypt

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
    admin <- findToken param
    case admin of
        Just (_,True ) -> do
            case getParam "id" param of
                Nothing -> throwError WrongQueryParameter
                Just uid -> do
                    env <- ask
                    let pool = dbConn env
                    _ <- liftIO $ execDB pool $ Query $
                        "DELETE FROM Users WHERE id = " <> uid
                    liftIO $ Logger.info (Logger.lConfig env) $ 
                        "Delete user id: " <> BS.toString uid
                    return $ A.String "Delete user"
        _ -> throwError NotFound
    
    let user = parseUser param
    env <- ask
    let pool = dbConn env
    oldUser <- liftIO $ queryDB pool $ (Query . BS.fromString) $
                "SELECT EXISTS (SELECT id FROM Users WHERE UserName = '"
                <> userName user
                <> "')"
    case fromOnly (head oldUser) of
        True  -> throwError UserExists
        False -> do
            let us = user {token = (BS.toString . makeHash . BS.fromString) $ 
                userName user <> upass user} 
            _ <- liftIO $ execDB pool $ (Query . BS.fromString) $
                "INSERT INTO Users "
                <> "(FirstName, LastName, Avatar, UserName, Pass, Token, RegDate)"
                <> "VALUES ("
                <> valUser user <> ", NOW ());"
            u <- liftIO $ queryDB pool $ (Query . BS.fromString) $
                "SELECT * FROM Users WHERE id>0 " 
                <> addFieldToQuery "UserName" (userName user)
                <> addFieldToQuery "FirstName" (firstName user)
                <> addFieldToQuery "LastName" (lastName user)
            liftIO $ Logger.info (Logger.lConfig env) $ "Add user: " <> userName user
            return $ A.toJSON (u :: [User])

userGet :: 
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
userGet param = do
    token <- findToken param
    if isNothing token
        then throwError NotFound
        else do
            env <- ask
            let user = parseUser param
            let pool = dbConn env
            u <- liftIO $ queryDB pool $ (Query . BS.fromString) $
                "SELECT * FROM Users WHERE id>0 " 
                <> addFieldToQuery "UserName" (userName user)
                <> addFieldToQuery "FirstName" (firstName user)
                <> addFieldToQuery "LastName" (lastName user)
                <> " ORDER BY UserName"
                <> getLimitOffset param
            return $ A.toJSON (u :: [User])

userDel :: 
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
userDel param = do
    admin <- findToken param
    case admin of
        Just (_,True ) -> do
            case getParam "id" param of
                Nothing -> throwError WrongQueryParameter
                Just uid -> do
                    env <- ask
                    let pool = dbConn env
                    _ <- liftIO $ execDB pool $ Query $
                        "DELETE FROM Users WHERE id = " <> uid
                    liftIO $ Logger.info (Logger.lConfig env) $ 
                        "Delete user id: " <> BS.toString uid
                    return $ A.String "Delete user"
        _ -> throwError NotFound