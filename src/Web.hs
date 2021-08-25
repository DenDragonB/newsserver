{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Web where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                   as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.ByteString.Lazy.UTF8    as BSLazy
import qualified Data.ByteString.UTF8         as BS

import           GHC.Generics
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp

import qualified DataBase
import qualified DataBase.Authors             as DataBase
import qualified DataBase.Categories          as DataBase
import qualified DataBase.Drafts              as DataBase
import qualified DataBase.Migration           as DataBase
import qualified DataBase.Photos              as DataBase
import qualified DataBase.Posts               as DataBase
import qualified DataBase.Tags                as DataBase
import qualified DataBase.Users               as DataBase
import           Exceptions
import qualified Logger

newtype Config = Config
    { port :: Int
    } deriving ( Show , Eq , Generic)
instance A.FromJSON Config

data Environment = Env
    Config
    Logger.Config
    DataBase.DBPool
    deriving Show
instance DataBase.HasDataBase Environment where
    dbConn (Env _ _ pool) = pool
instance Logger.HasLogger Environment where
    lConfig (Env _ clog _) = clog

sConfig :: Environment -> Config
sConfig (Env serv _ _) = serv

webRun :: ReaderT r m a -> r -> m a
webRun = runReaderT

start' :: ReaderT Environment IO ()
start' = do
    env <- ask
    let clog = Logger.lConfig env
    let p = (port . sConfig) env
    Logger.info clog $ "Start server at port " <> show p
    lift $ run p $ app env

start :: Environment -> IO ()
start env = do
    Logger.info (Logger.lConfig env) $ "Start server at port " <> show (port $ sConfig env)
    run (port $ sConfig env) $ app env

app :: Environment -> Application
app env request respond = do
    Logger.debug (Logger.lConfig env) $ "Request received: " <> show request
    resp <- runAnswear env request
    respond $ case resp of
        Right js         -> sendText js
        Left (Send file) -> sendFile file
        Left err         -> sendError err

sendError :: Errors -> Response
sendError err = responseLBS
    (errorCode err)
    [("Content-Type", "text/plain")]
    $ BSLazy.fromString $ show err

sendText :: A.Value -> Response
sendText js = responseLBS
    status200
    [("Content-Type", "text/plain")]
    $ A.encodingToLazyByteString $ A.pairs ("result" A..= ("Ok" :: String) <> "object" A..= js)

sendFile :: String -> Response
sendFile name = responseFile
    status200
    [("Content-Type", "text/plain")]
    name
    Nothing

type Answear = ReaderT Environment (ExceptT Errors IO) A.Value

runAnswear :: Environment -> Request -> IO (Either Errors A.Value)
runAnswear env req = runExceptT $ runReaderT (answear req) env

answear :: Request -> Answear
answear request = do
    env <- ask
    case (BS.toString . rawPathInfo) request of
        "/database.migrate" -> do
            liftIO $ DataBase.migrateDB (DataBase.dbConn env)
            return $ A.String "DataBase updated"
        -- Users API
        "/user.add"    -> DataBase.userAdd $ (paramToText . parseQuery . rawQueryString) request
        "/user.get"    -> DataBase.userGet $ (paramToText . parseQuery . rawQueryString) request
        "/user.delete" -> DataBase.userDel $ (paramToText . parseQuery . rawQueryString) request
        "/user.change_pass" -> DataBase.userNewPass $ (paramToText . parseQuery . rawQueryString) request
        -- Author API
        "/author.add"    -> DataBase.authorAdd $ (paramToText . parseQuery . rawQueryString) request
        "/author.edit"   -> DataBase.authorEdit $ (paramToText . parseQuery . rawQueryString) request
        "/author.get"    -> DataBase.authorGet $ (paramToText . parseQuery . rawQueryString) request
        "/author.delete" -> DataBase.authorDelete $ (paramToText . parseQuery . rawQueryString) request
        -- Category API
        "/category.add"    -> DataBase.categoryAdd $ (paramToText . parseQuery . rawQueryString) request
        "/category.edit"   -> DataBase.categoryEdit $ (paramToText . parseQuery . rawQueryString) request
        "/category.get"    -> DataBase.categoryGet $ (paramToText . parseQuery . rawQueryString) request
        "/category.delete" -> DataBase.categoryDelete $ (paramToText . parseQuery . rawQueryString) request
        -- Tags API
        "/tag.add"    -> DataBase.tagAdd $ (paramToText . parseQuery . rawQueryString) request
        "/tag.edit"   -> DataBase.tagEdit $ (paramToText . parseQuery . rawQueryString) request
        "/tag.get"    -> DataBase.tagGet $ (paramToText . parseQuery . rawQueryString) request
        "/tag.delete" -> DataBase.tagDelete $ (paramToText . parseQuery . rawQueryString) request
        -- Drafts API
        "/draft.add"    -> DataBase.draftAdd $ (paramToText . parseQuery . rawQueryString) request
        "/draft.edit"   -> DataBase.draftEdit $ (paramToText . parseQuery . rawQueryString) request
        "/draft.get"    -> DataBase.draftGet $ (paramToText . parseQuery . rawQueryString) request
        "/draft.delete" -> DataBase.draftDelete $ (paramToText . parseQuery . rawQueryString) request
        "/draft.publish" -> DataBase.draftPublish $ (paramToText . parseQuery . rawQueryString) request
        -- News API
        "/posts.get" -> DataBase.postGet $ (paramToText . parseQuery . rawQueryString) request
        -- Photos API
        str   -> if take 7 str == "/photos"
            then DataBase.fileGet $ tail str
            else throwError NotFound

paramToText :: [( BS.ByteString , Maybe BS.ByteString )] -> [( String , Maybe String )]
paramToText = map fun where
    fun (a,ma) = (BS.toString a, BS.toString <$> ma)
