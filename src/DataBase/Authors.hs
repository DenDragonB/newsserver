{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Authors where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import           Data.Maybe                         (listToMaybe, isJust)
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
    case admin of
        Just (_, True ) -> do
            uid <- parseParamM "user_id" param
            queryAbout <- getParamM "about" param

            let pool = dbConn env
            isUser <- existItemByField "Users" "Id" (uid :: Int)
            unless isUser (throwError UserNOTExists)
            isAuthor <- existItemByField "Authors" "UserId" (uid :: Int)
            when isAuthor (throwError ObjectExists)

            _ <- execWithExcept pool
                (Query "INSERT INTO Authors (UserId, About) VALUES (?,?);")
                (uid , queryAbout )
            liftIO $ Logger.info (Logger.lConfig env) $
                "Add author with user id: " <> show uid
            author <- queryWithExcept pool
                (Query "SELECT * FROM Authors WHERE UserId = ? ;")
                [uid]
            return $ A.toJSON $ listToMaybe (author :: [Author])
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
            aid <- parseParamM "id" param
            uid <- parseMaybeParam "user_id" param
            let queryAbout = getMaybeParam "about" param
            
            isAuthor <- existItemByField "Authors" "Id" (aid :: Int)
            unless isAuthor (throwError ObjectNOTExists)
            isUser <- existItemByField "Users" "Id" (uid :: Maybe Int)
            when (isJust uid && not isUser) (throwError UserNOTExists)

            let pool = dbConn env
            _ <- execWithExcept pool
                (Query "UPDATE Authors SET "
                    <> "UserId = COALESCE (?,userid),"
                    <> "About = COALESCE (?,about)"
                    <> "WHERE Id = ?;")
                (uid,queryAbout,aid)
            liftIO $ Logger.info (Logger.lConfig env) $
                "Edit author id: " <> show aid
            author <- queryWithExcept pool
                (Query "SELECT * FROM Authors WHERE Id = ? ;")
                [aid]
            return $ A.toJSON $ listToMaybe (author :: [Author])
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
            uid <- parseMaybeParam "user_id" param
            aid <- parseMaybeParam "id" param
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
            aid <- parseParamM "id" param
            isAuthor <- existItemByField "Authors" "Id" (aid :: Int)
            unless isAuthor (throwError ObjectNOTExists)

            let pool = dbConn env
            _ <- execWithExcept pool
                (Query "DELETE FROM Authors WHERE Id = ? ;")
                [aid]
            liftIO $ Logger.info (Logger.lConfig env) $
                "Delete author id: " <> show aid
            return $ A.String $ "Author with id "<> (pack . show) aid <>" deleted"
        _ -> throwError NotFound
    