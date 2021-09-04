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
import qualified Data.ByteString.UTF8       as BS
import           Text.Read                  (readMaybe)

import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Pool
import           Database.PostgreSQL.Simple

import qualified Exceptions
import qualified Logger
import GHC.Exception (Exception)

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
                throwError Exceptions.PostgreError
            Just (SqlError _ _ msg _ _) -> do
                liftIO . Logger.info (Logger.lConfig env)
                    $ BS.toString msg
                throwError Exceptions.PostgreError

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
                throwError Exceptions.PostgreError
            Just (SqlError _ _ msg _ _) -> do
                liftIO . Logger.info (Logger.lConfig env)
                    $ BS.toString msg
                throwError Exceptions.PostgreError

getLimitOffsetBS :: [( String , Maybe String )] -> BS.ByteString
getLimitOffsetBS param = limit <> offset
    where
        l = maybe (0 :: Int) read (getMaybeParam "limit" param)
        p = maybe (0 :: Int) read (getMaybeParam "page" param)
        limit = if l <= 0 then (""  :: BS.ByteString)
            else BS.fromString $ " LIMIT "  <> show l
        offset = if p <= 0 then ("" :: BS.ByteString)
            else BS.fromString $ " OFFSET " <> show (l*(p-1))

getMaybeParam :: String -> [( String , Maybe String )] -> Maybe String
getMaybeParam name = foldr (func name) Nothing
    where
        func nm (pn,pe) ini = if pn /= nm
            then ini
            else pe

getMaybeParamList :: String -> [( String , Maybe String )] -> Maybe [String]
getMaybeParamList name ps = splitCommas . delBracketStart . delBracketEnd <$> getMaybeParam name ps

parseMaybeParam ::
    ( MonadError Exceptions.Errors m
    , Read a)
    => String -> [( String , Maybe String )] -> m (Maybe a)
parseMaybeParam name prms = do
    case getMaybeParam name prms of
        Nothing -> return Nothing
        Just bspar -> case readMaybe bspar of
            Nothing -> throwError $ Exceptions.ParametrParseError name
            mpar    -> return mpar

parseMaybeParamList ::
    ( MonadError Exceptions.Errors m
    , Read a)
    => String -> [( String , Maybe String )] -> m (Maybe [a])
parseMaybeParamList name prms = do
    case getMaybeParamList name prms of
        Nothing -> return Nothing
        Just bspar -> case mapM readMaybe bspar of
            Nothing -> throwError $ Exceptions.ParametrParseError name
            mpar    -> return mpar
            
fromMaybeM :: (MonadError e m) => e -> Maybe a -> m a
fromMaybeM err = maybe (throwError err) pure

getParamM :: 
    ( MonadError Exceptions.Errors m) 
    => String -> [( String , Maybe String )] -> m String
getParamM name ps = fromMaybeM (Exceptions.WrongQueryParameter name) $ getMaybeParam name ps

parseParamM ::
    ( MonadError Exceptions.Errors m
    , Read a)
    => String -> [( String , Maybe String )] -> m a
parseParamM name prms = do
    bspar <- getParamM name prms
    case readMaybe bspar of
        Nothing -> throwError $ Exceptions.ParametrParseError name
        Just par -> return par

delBracketStart :: String -> String
delBracketStart xs = if isBracket x then tail xs else xs where
    x = listToMaybe xs

delBracketEnd :: String -> String
delBracketEnd xs = if isBracket x then init xs else xs where
    x = listToMaybe $ reverse xs

isBracket :: Maybe Char -> Bool
isBracket mc = case mc of
    Just c -> c `elem` ['[',']','{','}']
    _      -> False

splitCommas :: String -> [String]
splitCommas s = words $ map (\c -> if c == ',' then ' ' else c) s