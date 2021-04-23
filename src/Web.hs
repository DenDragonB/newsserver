{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web where

import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import           Data.Word

import           Control.Monad.Reader
import           Control.Monad.Except

import qualified Logger
import qualified DataBase

data Config = Config
    { port :: Int 
    } deriving ( Show , Eq )
instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Web.Config" $ \o -> 
        Config <$> o A..: "port"
--               <*> o A..: "logPath" 
--               <*> o A..: "logMinLevel" 

data Environment = Env
    { sConfig  :: Config
    , lConfig  :: Logger.Config 
    , dbConfig :: DataBase.Config
    } deriving ( Show , Eq )

start :: Environment -> IO ()
start env @ Env {..} = do
    Logger.info lConfig $ "Start server at port " <> show (port sConfig)
    run (port sConfig) $ app env

app :: Environment -> Application
app env @ Env {..} request respond = do
    Logger.info lConfig $ "Request received: " <> show request
    respond $ case rawPathInfo request of
        "/posts" -> sendText 
        _        -> notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

sendText :: Response
sendText = responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello world!"

type Answear = ReaderT Environment (Except String) A.Value

runAnswear env req = runReaderT (answear req) env

answear :: Request -> Answear
answear request = do
    let (entity,param) = BS.break (== ('?' :: Word8) ) $ rawPathInfo request
    case entity of
        "/post" -> return $ A.toJSON "Hello word"
        "/" -> lift $ fail "Unknown request"
        _   -> lift $ fail "Not Found"


