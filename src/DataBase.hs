{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DataBase where

import qualified Data.Aeson                 as A
import           GHC.Generics

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Conversion as BS
import qualified Data.ByteString.UTF8       as BS
import           Data.Int
import           Data.Maybe
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple

import qualified Exceptions
import qualified Logger

data Config = Config
    { host   :: String
    , port   :: Int
    , dbname :: String
    , user   :: String
    , pass   :: String
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
                , connectDatabase = dbname
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
        func nm (pn,pe) ini = if pn /= nm
            then ini
            else pe

parseParam ::
    ( MonadError Exceptions.Errors m
    , BS.FromByteString a)
    => BS.ByteString -> [( BS.ByteString , Maybe BS.ByteString )] -> m (Maybe a)
parseParam name prms = do
    case getParam name prms of
        Nothing -> return Nothing
        Just bspar -> case BS.fromByteString bspar of
            Nothing -> throwError (Exceptions.ParametrParseError $ BS.toString name)
            mpar -> return mpar

parseParamDelBracket ::
    ( MonadError Exceptions.Errors m
    , BS.FromByteString a)
    => BS.ByteString -> [( BS.ByteString , Maybe BS.ByteString )] -> m (Maybe a)
parseParamDelBracket name prms = do
    case getParam name prms of
        Nothing -> return Nothing
        Just bspar -> case (BS.fromByteString . delBracketStart . delBracketEnd) bspar of
            Nothing -> throwError (Exceptions.ParametrParseError $ BS.toString name)
            mpar -> return mpar

parseParamDay ::
    MonadError Exceptions.Errors m
    => BS.ByteString -> [( BS.ByteString , Maybe BS.ByteString )] -> m (Maybe Day)
parseParamDay name prms = do
    case getParam name prms of
        Nothing -> return Nothing
        Just bspar -> case toDay bspar of
            Nothing -> throwError (Exceptions.ParametrParseError $ BS.toString name)
            mpar -> return mpar

queryWithExcept ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ,ToRow q, FromRow r) => DBPool -> Query -> q -> m [r]
queryWithExcept pool querystring q = do
    env <- ask
    res <- liftIO $ try (queryDBsafe pool querystring q)
    case res of
        Right out -> return out
        Left err -> case fromException err of
            Nothing -> do
                liftIO . Logger.info (Logger.lConfig env) $ show err
                throwError Exceptions.WrongQueryParameter
            Just (SqlError _ _ msg _ _) -> do
                liftIO . Logger.info (Logger.lConfig env)
                    $ BS.toString msg
                throwError Exceptions.WrongQueryParameter

execWithExcept ::
    ( MonadReader env m
    , HasDataBase env
    , Logger.HasLogger env
    , MonadError Exceptions.Errors m
    , MonadIO m
    ,ToRow q) => DBPool -> Query -> q -> m Int64
execWithExcept pool querystring q = do
    env <- ask
    res <- liftIO $ try (execDBsafe pool querystring q)
    case res of
        Right out -> return out
        Left err -> case fromException err of
            Nothing -> do
                liftIO . Logger.info (Logger.lConfig env) $ show err
                throwError Exceptions.WrongQueryParameter
            Just (SqlError _ _ msg _ _) -> do
                liftIO . Logger.info (Logger.lConfig env)
                    $ BS.toString msg
                throwError Exceptions.WrongQueryParameter

delBracketStart :: BS.ByteString -> BS.ByteString
delBracketStart bs = if isBracket (BS.take 1 bs) then BS.drop 1 bs else bs

delBracketEnd :: BS.ByteString -> BS.ByteString
delBracketEnd bs = if isBracket (BS.drop (BS.length bs - 1) bs) then BS.take (BS.length bs - 1) bs else bs

isBracket :: BS.ByteString -> Bool
isBracket bs = case BS.decode bs of
    Just (c,_) -> c `elem` ['[',']','{','}']
    _          -> False

toDay :: BS.ByteString -> Maybe Day
toDay bs =
    let year = fromMaybe 0 $ BS.fromByteString $ BS.take 4 bs
        month = fromMaybe 0 $ BS.fromByteString $ (BS.take 2 . snd . BS.splitAt 5) bs
        day = fromMaybe 0 $ BS.fromByteString $ BS.drop 8 bs
    in fromGregorianValid year month day
