{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DataBase where

import qualified Data.Aeson                       as A
import           GHC.Generics

import           Control.Monad.Reader
import qualified Data.ByteString.Conversion       as BS
import qualified Data.ByteString.UTF8             as BS
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types

import qualified Logger

import           Data.Text.Encoding
import           Data.Text.IO                     as TIO

data Config = Config
    { host :: String
    , port :: Int
    , name :: String
    , user :: String
    , pass :: String
    } deriving (Show,Eq,Generic)
instance A.FromJSON Config

type DBPool = Pool Connection

class HasDataBase env where
    dbConn :: env -> DBPool

class Monad m => MyDatabase m where
    openPool :: Config -> m DBPool
    queryDB  :: FromRow r => DBPool -> Query -> m [r]
    execDB   :: DBPool -> Query -> m Int64
    queryDBsafe :: (ToRow q, FromRow r) => DBPool -> Query -> q -> m [r]
    execDBsafe :: ToRow q => DBPool -> Query -> q -> m Int64

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
    queryDBsafe pool qstring qdata = withResource pool $ \conn -> query conn qstring qdata
    execDBsafe pool qstring qdata = withResource pool $ \conn -> execute conn qstring qdata

getLimitOffset :: [( BS.ByteString , Maybe BS.ByteString )] -> String
getLimitOffset param = limit <> offset
    where
        l = fromMaybe (0 :: Int) (getParam "limit" param >>= BS.fromByteString)
        p = fromMaybe (0 :: Int) (getParam "page" param >>= BS.fromByteString)
        limit = if l <= 0 then ""
            else " LIMIT " <> show l
        offset = if p <= 0 then ""
            else " OFFSET " <> show (l*(p-1))

getLimitOffsetBS :: [( BS.ByteString , Maybe BS.ByteString )] -> BS.ByteString
getLimitOffsetBS param = limit <> offset
    where
        l = fromMaybe (0 :: Int) (getParam "limit" param >>= BS.fromByteString)
        p = fromMaybe (0 :: Int) (getParam "page" param >>= BS.fromByteString)
        limit = if l <= 0 then (""  :: BS.ByteString)
            else BS.fromString $ " LIMIT "  <> show l
        offset = if p <= 0 then ("" :: BS.ByteString)
            else BS.fromString $ " OFFSET " <> show (l*(p-1))

getParam :: BS.ByteString -> [( BS.ByteString , Maybe BS.ByteString )] -> Maybe BS.ByteString
getParam name = foldr (func name) Nothing
    where
        func name (pn,pe) ini = if pn /= name
            then ini
            else pe

addFieldToQuery :: String -> String -> String
addFieldToQuery field val = if null val
    then ""
    else " AND " <> field <> " = '" <> val <> "'"

addFieldToQueryBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
addFieldToQueryBS field val = if val == ""
    then ""
    else " AND " <> field <> " = '" <> val <> "'"

addFieldToQueryNumBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
addFieldToQueryNumBS field val = if val == ""
    then ""
    else " AND " <> field <> " = " <> val

addFieldToQueryLaterBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
addFieldToQueryLaterBS field val = if val == ""
    then ""
    else " AND " <> field <> " < '" <> val <> "'"

addFieldToQueryGraterBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
addFieldToQueryGraterBS field val = if val == ""
    then ""
    else " AND " <> field <> " > '" <> val <> "'"

addFindTextToSelectBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
addFindTextToSelectBS field val = if val == ""
    then ""
    else " AND strpos("<> field <> ",'" <> val <> "') > 0"

addToUpdate :: BS.ByteString -> BS.ByteString -> BS.ByteString
addToUpdate field val = if val == ""
    then ""
    else ", " <> field <> " = '" <> val <> "'"

addToUpdateNum :: BS.ByteString -> BS.ByteString -> BS.ByteString
addToUpdateNum field val = if val == ""
    then ""
    else ", " <> field <> " = " <> val

addToUpdateNumArray :: BS.ByteString -> BS.ByteString -> BS.ByteString
addToUpdateNumArray field val = if val == ""
    then ""
    else ", " <> field <> " = ARRAY" <> val

addToUpdateNumArrayMap :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
addToUpdateNumArrayMap field vals = if null vals
    then ""
    else ", " <> field <>
        " = ARRAY[" <> (BS.drop 1 . foldr (\v ini -> ini<>","<>v) "") vals <> "]"

addToUpdateArrayMap :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
addToUpdateArrayMap field vals = if null vals
    then ""
    else ", " <> field <>
        " = ARRAY[" <> (BS.drop 1 . foldr (\v ini -> ini<>",'"<>v<>"'") "") vals <> "]"
