{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.Aeson   as Aeson
import qualified Data.Text    as T
import qualified Text.Toml    as TOML

import qualified DataBase
import           GHC.Generics
import qualified Logger
import qualified Web

data Config = Config
    { logger   :: Logger.Config
    , database :: DataBase.Config
    , server   :: Web.Config
    } deriving (Show,Eq,Generic)
instance Aeson.FromJSON Config

main :: IO ()
main = do
    -- read config
    tConf <- readFile "config.toml"
    let toml = TOML.parseTomlDoc "" $ T.pack tConf
    case toml of
        Left err -> fail $ show err
        Right t  -> do
            let conf = Aeson.eitherDecode $ Aeson.encode t :: Either String Config
            case conf of
                Left err -> fail err
                Right Config {..} -> do
                    pool <- DataBase.openPool database
                    Web.start $ Web.Env server logger pool
