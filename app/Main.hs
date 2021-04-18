{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Text.Toml as TOML

import qualified Logger
import qualified DataBase


data Config = Config
    { logConfig :: Logger.Config
    , dbConfig  :: DataBase.Config
    } deriving (Show,Eq)
instance Aeson.FromJSON Config where
    parseJSON = Aeson.withObject "FromJSON Main.Config" $ \o -> 
        Config  <$> o Aeson..: "logger"   
                <*> o Aeson..: "database"

main :: IO ()
main = do
    -- read config
    tConf <- readFile "config.toml"
    let toml = TOML.parseTomlDoc "" $ T.pack $ tConf
    case toml of
        Left err -> fail $ show err
        Right t  -> do
            let conf = Aeson.eitherDecode $ Aeson.encode t :: Either String Config
            case conf of
                Left err -> fail err
                Right config -> print config
