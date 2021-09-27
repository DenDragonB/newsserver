{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Postgres where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.UTF8                 as BS
import           Data.Maybe                           (listToMaybe)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.ToField   (ToField)
import           Database.PostgreSQL.Simple.Types
import           Exceptions

import           DataBase
import           Logger

existItemByField ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    , ToField a
    ) => String -> String -> a -> m Bool
existItemByField table field value = do
    isExist <- queryWithExcept
                (Query $ "SELECT EXISTS (SELECT id FROM "
                    <> BS.fromString table <> " WHERE "
                    <> BS.fromString field <> " = ?);")
                [value]
    return (maybe False fromOnly $ listToMaybe isExist)

selectMaybeItemByField ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    , ToField a, FromRow b
    ) => String -> String -> a -> m (Maybe b)
selectMaybeItemByField table field value = do
    items <- queryWithExcept
                (Query $ "SELECT * FROM "
                    <> BS.fromString table <> " WHERE "
                    <> BS.fromString field <> " = ?);")
                [value]
    return $ listToMaybe items
