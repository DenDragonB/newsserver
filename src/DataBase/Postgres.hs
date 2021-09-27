{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Postgres where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
import           Database.PostgreSQL.Simple.ToField (ToField)
import qualified Data.ByteString.UTF8             as BS
import           Control.Monad.Except
import           Control.Monad.Reader
import           Exceptions
import Data.Maybe ( listToMaybe )

import           Logger
import           DataBase

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