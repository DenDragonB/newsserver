{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module DataBase.Tags where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types


import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import qualified Data.ByteString.UTF8               as BS
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)

import           DataBase
import           DataBase.Users
import           Exceptions
import           Logger

data Tag = Tag
    { tid     :: Int
    , tagName :: Text
    } deriving (Show,Eq)
instance A.ToJSON Tag where
    toJSON tag = A.object
        [ "id"        A..= tid tag
        , "name"      A..= tagName tag
        ]
instance FromRow Tag where
    fromRow = Tag
        <$> field
        <*> field

tagAdd ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
tagAdd param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let mname = getParam "name" param
            case mname of
                Nothing -> throwError WrongQueryParameter
                Just name -> do
                    let pool = dbConn env
                    isTag <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Tags WHERE Tag = '" <> name <> "');"
                    when (fromOnly $ head isTag) (throwError ObjectExists)
                    _ <- liftIO $ execDB pool $ Query $
                        "INSERT INTO Tags (Tag) VALUES"
                        <> "('" <> name <> "');"
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Add Tag: " <> BS.toString name
                    tag <- liftIO $ queryDB pool $ Query $
                        "SELECT * FROM Tags WHERE Tag = '" <> name <> "';"
                    return $ A.toJSON (tag :: [Tag])
        _ -> throwError NotFound

tagEdit ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
tagEdit param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let mtid = getParam "id" param
            let mname = getParam "name" param
            case sequence [mtid,mname] of
                Just [tid,tname] -> do
                    let pool = dbConn env
                    isTag <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Tags WHERE Id = " <> tid <> ");"
                    isTagName <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Tags WHERE Tag = '" <> tname <> "');"
                    unless (fromOnly $ head isTag) (throwError ObjectNOTExists)
                    when (fromOnly $ head isTagName) (throwError ObjectExists)
                    _ <- liftIO $ execDB pool $ Query $
                        "UPDATE Tags SET Id = " <> tid
                        <> addToUpdate "Tag" tname
                        <> " WHERE Id = " <> tid <> ";"
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Edit Tag id: " <> BS.toString tid
                    tag <- liftIO $ queryDB pool $ Query $
                        "SELECT * FROM Tags WHERE Id = " <> tid <> ";"
                    return $ A.toJSON (tag :: [Tag])
                _ -> throwError WrongQueryParameter
        _ -> throwError NotFound

tagGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
tagGet param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (True , _ ) -> do
            let pool = dbConn env
            let tid = fromMaybe "" $ getParam "id" param
            let tname = fromMaybe "" $ getParam "name" param
            tag <- liftIO $ queryDB pool $ Query $
                "SELECT * FROM Tags WHERE Id > 0 "
                <> addFieldToQueryNumBS "Id" tid
                <> addFieldToQueryBS "Tag" tname
                <> getLimitOffsetBS param
                <> ";"
            return $ A.toJSON (tag :: [Tag])
        _ -> throwError NotFound

tagDelete ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
tagDelete param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let mtid = getParam "id" param
            case mtid of
                Nothing -> throwError WrongQueryParameter
                Just tid -> do
                    let pool = dbConn env
                    isTag <- liftIO $ queryDB pool $ Query $
                        "SELECT EXISTS (SELECT id FROM Tags WHERE Id = " <> tid <> ");"
                    unless (fromOnly $ head isTag) (throwError ObjectNOTExists)
                    _ <- liftIO $ execDB pool $ Query $
                        "DELETE FROM Tags WHERE Id = "<> tid <> ";"
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Delete Tag id: " <> BS.toString tid
                    return $ A.String $ decodeUtf8 $ "Tag with id " <> tid <> " deleted"
        _ -> throwError NotFound
