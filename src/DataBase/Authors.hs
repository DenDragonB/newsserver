{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Authors where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import           Data.Maybe                         (isJust, listToMaybe)
import           Data.Text                          (Text, pack)
import           GHC.Generics

import           DataBase
import           DataBase.Postgres
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
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            uid <- parseParamM "user_id" param
            queryAbout <- getParamM "about" param

            isUser <- existItemByField "Users" "Id" (uid :: Int)
            unless isUser (throwError UserNOTExists)
            isAuthor <- existItemByField "Authors" "UserId" (uid :: Int)
            when isAuthor (throwError ObjectExists)

            _ <- execWithExcept
                (Query "INSERT INTO Authors (UserId, About) VALUES (?,?);")
                (uid , queryAbout )
            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Add author with user id: " <> show uid
            author <- selectMaybeItemByField "Authors" "UserId" uid
            return $ A.toJSON (author :: Maybe Author)
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

            _ <- execWithExcept
                (Query "UPDATE Authors SET "
                    <> "UserId = COALESCE (?,userid),"
                    <> "About = COALESCE (?,about)"
                    <> "WHERE Id = ?;")
                (uid,queryAbout,aid)
            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Edit author id: " <> show aid
            author <- selectMaybeItemByField "Authors" "Id" aid
            return $ A.toJSON (author :: Maybe Author)
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
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            uid <- parseMaybeParam "user_id" param
            aid <- parseMaybeParam "id" param
            author <- queryWithExcept
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
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            aid <- parseParamM "id" param
            isAuthor <- existItemByField "Authors" "Id" (aid :: Int)
            unless isAuthor (throwError ObjectNOTExists)

            _ <- execWithExcept
                (Query "DELETE FROM Authors WHERE Id = ? ;")
                [aid]
            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Delete author id: " <> show aid
            return $ A.String $ "Author with id "<> (pack . show) aid <>" deleted"
        _ -> throwError NotFound

