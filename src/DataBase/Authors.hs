{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Authors where

import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                       as A
import           Data.Maybe                       (isJust)
import           Data.Text                        (Text, pack)
import           GHC.Generics

import           DataBase
import           DataBase.Postgres
import           DataBase.Types                   (Author)
import           DataBase.Users
import           Exceptions
import           Logger

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

            isUser <- existItemByField Users Id (uid :: Int)
            unless isUser (throwError UserNOTExists)
            isAuthor <- existItemByField Authors UserId (uid :: Int)
            when isAuthor (throwError ObjectExists)

            author <- insertAuthorWithPrameters uid queryAbout

            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Add author with user id: " <> show uid

            return $ A.toJSON author
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

            isAuthor <- existItemByField Authors Id aid
            unless isAuthor (throwError ObjectNOTExists)
            isUser <- existItemByField Users Id uid
            when (isJust uid && not isUser) (throwError UserNOTExists)

            author <- updateAuthorWithParameters aid uid queryAbout

            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Edit author id: " <> show aid

            return $ A.toJSON author
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
            author <- getAuthorWithParameters aid uid param
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
            isAuthor <- existItemByField Authors Id aid
            unless isAuthor (throwError ObjectNOTExists)

            deleteItemFromTableById Authors aid

            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                "Delete author id: " <> show aid
            return $ A.String $ "Author with id "<> (pack . show) aid <>" deleted"
        _ -> throwError NotFound

