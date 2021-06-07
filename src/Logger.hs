{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger where

import           Prelude hiding (log, error)
import qualified System.IO as IO (IOMode (..),FilePath,withFile,hPutStrLn,stderr)
import qualified Data.Aeson as A
import qualified Data.Text as T
import           Data.Time.Clock 

import           Control.Monad.Reader

class HasLogger env where
    lConfig :: env -> Config

class Monad m => MonadLog m where
    debug   :: Config -> String -> m ()
    info    :: Config -> String -> m ()
    warning :: Config -> String -> m ()
    error   :: Config -> String -> m ()
instance MonadLog IO where
    debug   c = log c Debug
    info    c = log c Info
    warning c = log c Warning
    error   c = log c Error
instance MonadIO m => MonadLog (ReaderT r m) where 
    debug   c str = liftIO $ log c Debug str
    info    c = liftIO . log c Info
    warning c = liftIO . log c Warning
    error   c = liftIO . log c Error


data LogLevel 
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)
instance A.FromJSON LogLevel where
    parseJSON = A.withText "FromJSON Logger.LogLevel" $ \t ->
        case t of
            "Debug"   -> pure Debug
            "Info"    -> pure Info
            "Warning" -> pure Warning
            "Error"   -> pure Error
            _         -> fail $ "Unknown log level: " ++ T.unpack t

data LogTo
    = LogToFile
    | LogToConsole
    deriving (Eq, Ord, Show)
instance A.FromJSON LogTo where
    parseJSON = A.withText "FromJSON Logger.LogTo" $ \t ->
        case t of
            "LogToFile"    -> pure LogToFile
            "LogToConsole" -> pure LogToConsole
            _         -> fail $ "Unknown where log to: " ++ T.unpack t

data Config = Config
    { logTo   :: LogTo
    , logPath :: IO.FilePath
    , logMinLevel :: LogLevel
    } deriving (Show,Eq)
instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Logger.Config" $ \o -> 
        Config <$> o A..: "logTo"
               <*> o A..: "logPath" 
               <*> o A..: "logMinLevel" 

-- Output log string to file
logToFile :: Config -> LogLevel -> String -> IO ()
logToFile Config {..} lvl str = IO.withFile logPath IO.AppendMode 
    (\ hdl -> do
        time <- getCurrentTime
        IO.hPutStrLn hdl $ show time ++ " - " ++ show lvl ++ ": " ++ str)

-- Output log string to stderr
logToConsole :: LogLevel -> String -> IO ()
logToConsole lvl str = IO.hPutStrLn IO.stderr $ show lvl ++ ": " ++ str

-- Output log depending on the configuration
log :: Config -> LogLevel -> String -> IO ()
log conf lvl str | lvl >= logMinLevel conf = case logTo conf of
                                                           LogToFile -> logToFile conf lvl str
                                                           _         -> logToConsole lvl str
                 | otherwise               = return ()