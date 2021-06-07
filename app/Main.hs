{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Text.Toml as TOML

import qualified Logger
import qualified DataBase
import qualified Web


data Config = Config
    { logConfig :: Logger.Config
    , dbConfig  :: DataBase.Config
    , webConfig :: Web.Config
    } deriving (Show,Eq)
instance Aeson.FromJSON Config where
    parseJSON = Aeson.withObject "FromJSON Main.Config" $ \o -> 
        Config  <$> o Aeson..: "logger"   
                <*> o Aeson..: "database"
                <*> o Aeson..: "server"


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
                Right Config {..} -> do
                    pool <- DataBase.openPool dbConfig
                    Web.start $ Web.Env webConfig logConfig pool
                -- Right Config {..} -> Web.webRun Web.start' $ Web.Env webConfig logConfig dbConfig