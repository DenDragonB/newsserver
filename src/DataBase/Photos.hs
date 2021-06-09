{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module DataBase.Photos where

import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Aeson as A
import qualified System.Directory as IO

import           Exceptions
import           Logger

fileGet :: 
    ( MonadReader env m
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => String
    -> m A.Value
fileGet fileName = do
    env <- ask
    isFile <- liftIO $ IO.doesFileExist fileName
    if isFile 
        then do 
            liftIO $ Logger.debug (Logger.lConfig env) $
                "File " <> fileName <> " will be send"
            throwError $ Send fileName
        else do
            liftIO $ Logger.debug (Logger.lConfig env) $
                "File " <> fileName <> " not found"
            throwError NotFound
