{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Logger
import qualified Data.Aeson as A

data Config = Config
    { nameDB :: String 
    , adminDB :: String
    , passDB :: String       
    }
instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Server.Config" $ \o -> 
        Config <$> o A..: "name"
               <*> o A..: "admin" 
               <*> o A..: "pass" 