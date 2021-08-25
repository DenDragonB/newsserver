{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Authors where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import           Data.Maybe                         (listToMaybe)
import           Data.Text                          (Text, pack)
import           GHC.Generics

import           DataBase
import           DataBase.Users
import           Exceptions
import           Logger

data Author = Author
    { id      :: Int
    , user_id :: Int
    , about   :: Text
    } deriving (Show,Eq,Generic)
instance A.ToJSON Author
instance FromRow Author

authorAdd ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
authorAdd param = do
    env <- ask
    admin <- findToken param
    liftIO $ Logger.debug (Logger.lConfig env) $
        "Just (User,Admin): " <> show admin
    case admin of
        Just (_, True ) -> do
            muid <- parseParam "user_id" param
            let mabout = getParam "about" param
            case sequence (muid :: Maybe Int,mabout) of
                Just (uid,queryAbout) -> do
                    let pool = dbConn env
                    isUser <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Users WHERE id = ?);")
                        [uid]
                    unless (maybe False fromOnly $ listToMaybe isUser) (throwError UserNOTExists)
                    isAuthor <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Authors WHERE UserId = ?);")
                        [uid]
                    when (maybe False fromOnly $ listToMaybe isAuthor) (throwError ObjectExists)
                    _ <- execWithExcept pool
                        (Query "INSERT INTO Authors (UserId, About) VALUES (?,?);")
                        (uid , queryAbout )
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Add author with user id: " <> show uid
                    author <- queryWithExcept pool
                        (Query "SELECT * FROM Authors WHERE UserId = ? ;")
                        [uid]
                    return $ A.toJSON (author :: [Author])
                _ -> throwError WrongQueryParameter
        _ -> throwError NotFound

authorEdit ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
authorEdit param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            uid <- parseParam "user_id" param
            let queryAbout = getParam "about" param
            maid <- parseParam "id" param
            case maid :: Maybe Int of
                Nothing -> throwError WrongQueryParameter
                Just aid -> do
                    let pool = dbConn env
                    isAuthor <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Authors WHERE Id = ?);")
                        [aid]
                    unless (maybe False fromOnly $ listToMaybe isAuthor) (throwError ObjectNOTExists)
                    _ <- execWithExcept pool
                        (Query "UPDATE Authors SET "
                            <> "UserId = COALESCE (?,userid),"
                            <> "About = COALESCE (?,about)"
                            <> "WHERE Id = ?;")
                        (uid :: Maybe Int,queryAbout,aid)
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Edit author id: " <> show aid
                    author <- queryWithExcept pool
                        (Query "SELECT * FROM Authors WHERE Id = ? ;")
                        [aid]
                    return $ A.toJSON (author :: [Author])
        _ -> throwError NotFound

authorGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
authorGet param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let pool = dbConn env
            uid <- parseParam "user_id" param
            aid <- parseParam "id" param
            author <- queryWithExcept pool
                (Query $ "SELECT id,userid,about FROM authors WHERE"
                    <> "(id = COALESCE (?, id))"
                    <> "AND (userid = COALESCE (?, userid))"
                    <> "ORDER BY id "
                    <> getLimitOffsetBS param <> ";")
                (aid :: Maybe Int, uid :: Maybe Int)
            return $ A.toJSON (author :: [Author])
        _ -> throwError NotFound

authorDelete ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
authorDelete param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            maid <- parseParam "id" param
            case maid :: Maybe Int of
                Nothing -> throwError WrongQueryParameter
                Just aid -> do
                    let pool = dbConn env
                    isAuthor <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Authors WHERE Id = ?);")
                        [aid]
                    unless (maybe False fromOnly $ listToMaybe isAuthor) (throwError ObjectNOTExists)
                    _ <- execWithExcept pool
                        (Query "DELETE FROM Authors WHERE Id = ? ;")
                        [aid]
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Delete author id: " <> show aid
                    return $ A.String $ "Author with id "<> (pack . show) aid <>" deleted"
        _ -> throwError NotFound
