{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module DataBase.Photos where

import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Aeson as A
import qualified Data.ByteString.UTF8 as BS
import           Data.Text ( Text )
import           Data.Text.Encoding (decodeUtf8,encodeUtf8)
import           Data.Maybe (fromMaybe)
import           Data.Time

import           DataBase
import           DataBase.Users
import           Exceptions
import           Logger

fileGet :: 
    ( MonadReader env m
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => BS.ByteString
    -> m A.Value
fileGet param = do
    env <- ask
    throwError $ Send "param"
