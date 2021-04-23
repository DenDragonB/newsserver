{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module DataBase where

import qualified Data.Aeson as A
import           Database.PostgreSQL.Simple
import           Data.Pool
import           Control.Monad.Reader
import           Data.Int

import qualified Logger

data Config = Config
    { host :: String
    , port :: Int
    , name :: String 
    , user :: String
    , pass :: String       
    } deriving (Show,Eq)
instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Server.Config" $ \o -> 
        Config <$> o A..: "host"
               <*> o A..: "port" 
               <*> o A..: "name" 
               <*> o A..: "user" 
               <*> o A..: "pass"

type DBPool = Pool Connection

class HasDataBase env where
    dbConn :: env -> DBPool

class Monad m => MyDatabase m where
    openPool :: Config -> m (Pool Connection)
    queryDB  :: FromRow r => DBPool -> Query -> m [r]
    execDB   :: DBPool -> Query -> m Int64
instance MyDatabase IO where
    openPool Config {..} = do
        let cInfo = ConnectInfo 
                { connectHost = host
                , connectPort = toEnum port
                , connectUser = user
                , connectPassword = pass
                , connectDatabase = name
                }
        createPool
            (connect cInfo)
            close
            1   -- Number of sub-pools
            5   -- Seconds to keep a resource open
            4   -- Number of resources per sub-pool
    queryDB pool q = withResource pool $ \conn -> query_ conn q
    execDB pool q = withResource pool $ \conn -> execute_ conn q


q :: Query
q = "CREATE TABLE users ("
    <> "id INT PRIMARY KEY NOT NULL,"
    <> "name TEXT NOT NULL"
    <> ");"