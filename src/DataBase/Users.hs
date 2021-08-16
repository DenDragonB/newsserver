{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Users where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import           Crypto.BCrypt
import qualified Data.Aeson                         as A
import qualified Data.ByteString.Conversion         as BS
import qualified Data.ByteString.UTF8               as BS
import           Data.Maybe
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Time
import           GHC.Generics

import           DataBase
import           Exceptions
import           Logger
import           Prelude                            hiding (id)

data User = User
    { id         :: Int
    , first_name :: String
    , last_name  :: String
    , avatar     :: String
    , name       :: String
    , upass      :: String
    , reg_date   :: Day
    , adm        :: Bool
    , token      :: String
    } deriving (Show,Eq,Generic)
instance A.ToJSON User
instance FromRow User

emptyUser :: User
emptyUser = User
    { id = 0
    , first_name = ""
    , last_name = ""
    , avatar = ""
    , name = ""
    , upass = ""
    , reg_date = ModifiedJulianDay 1
    , adm = False
    , token = ""
    }

parseUser :: [( BS.ByteString , Maybe BS.ByteString )] -> User
parseUser = foldr func emptyUser where
    func (pn,pe) puser = case pn of
        "id"         -> puser {id = fromMaybe 0 (pe >>= BS.fromByteString)}
        "last_name"  -> puser {last_name = BS.toString $ fromMaybe "" pe}
        "first_name" -> puser {first_name = BS.toString $ fromMaybe "" pe}
        "name"       -> puser {name = BS.toString $ fromMaybe "" pe}
        "avatar"     -> puser {avatar = BS.toString $ fromMaybe "" pe}
        "pass"       -> puser {upass = BS.toString $ makeHash $ fromMaybe "" pe}
        "token"      -> puser {token = BS.toString $ fromMaybe "" pe}
        _            -> puser

makeHash :: BS.ByteString -> BS.ByteString
makeHash bs = fromMaybe "" $ hashPassword bs $
    fromMaybe "" $ genSalt "$2b$" 6 "qsfvkpkvtnhtefhk"

userAdd ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
userAdd param = do
    let userToAdd = parseUser param
    env <- ask
    let pool = dbConn env
    oldUser <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Users WHERE UserName = ? );")
                [name userToAdd]
    when (maybe False fromOnly $ listToMaybe oldUser) (throwError ObjectExists)
    _ <- execWithExcept pool
        (Query "INSERT INTO Users "
            <> "(FirstName, LastName, Avatar, UserName, Pass, Token, RegDate)"
            <> "VALUES (?,?,?,?,md5(?),md5(?),NOW());")
        (first_name userToAdd, last_name userToAdd, avatar userToAdd, name userToAdd, upass userToAdd,
            (BS.toString . makeHash . BS.fromString) (name userToAdd <> upass userToAdd))
    u <- queryWithExcept pool
        (Query "SELECT * FROM Users WHERE UserName = ? AND FirstName = ? AND LastName = ?;")
        (name userToAdd, first_name userToAdd, last_name userToAdd)
    liftIO $ Logger.info (Logger.lConfig env) $ "Add user: " <> name userToAdd
    return $ A.toJSON (u :: [User])

findToken ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m (Maybe (Bool,Bool))
findToken param = do
    env <- ask
    let queryToken = getParam "token" param
    liftIO $ Logger.debug (Logger.lConfig env) $
                        "Request from user with token: " <> show queryToken
    if isNothing queryToken
        then return Nothing
        else do
            let pool = dbConn env
            userExist <- queryWithExcept pool
                    (Query "SELECT EXISTS (SELECT id FROM Users WHERE token = ? );")
                    [fromMaybe "" queryToken]
            admExist <- queryWithExcept pool
                    (Query "SELECT Adm FROM Users WHERE token = ? ;")
                    [fromMaybe "" queryToken]
            case admExist of
                [] -> return $ Just ( maybe False fromOnly $ listToMaybe userExist , False)
                _  -> return $ Just ( maybe False fromOnly $ listToMaybe userExist , maybe False fromOnly $ listToMaybe admExist)

userGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
userGet param = do
    queryToken <- findToken param
    if isNothing queryToken
        then throwError NotFound
        else do
            env <- ask
            let queryUser = parseUser param
            let pool = dbConn env
            u <- queryWithExcept pool
                (Query $ "WITH searchData AS (SELECT "
                    <> " CAST (? as TEXT) AS sUserName "
                    <> ", CAST (? as TEXT) AS sFirstName "
                    <> ", CAST (? as TEXT) AS sLastName ) "
                    <> "SELECT id,UserName,FirstName,LastName,Avatar,RegDate FROM Users,searchData WHERE"
                    <> "(sUserName ISNULL OR sUserName=UserName)"
                    <> "AND (sFirstName ISNULL OR sFirstName=FirstName)"
                    <> "AND (sLastName ISNULL OR sLastName=LastName)"
                    <> "ORDER BY UserName "
                    <> getLimitOffsetBS param <> ";")
                (name queryUser,first_name queryUser,last_name queryUser)
            return $ A.toJSON (u :: [User])

userDel ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
userDel param = do
    admin <- findToken param
    case admin of
        Just (_,True ) -> do
            case getParam "id" param of
                Nothing -> throwError WrongQueryParameter
                Just uid -> do
                    env <- ask
                    let pool = dbConn env
                    _ <- execWithExcept pool
                        (Query "DELETE FROM Users WHERE id = ? ;")
                        [uid]
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Delete user id: " <> BS.toString uid
                    return $ A.String $ decodeUtf8 $ "User with id "<>uid<>" deleted"
        _ -> throwError NotFound

userNewPass ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
userNewPass param = do
    env <- ask
    let mUserName = getParam "name" param
    let mPass = getParam "pass" param
    let mNewPass = getParam "new_pass" param
    case sequence [mUserName,mPass,mNewPass] of
        Just [uName,uPass,uNewPass] -> do
            let pool = dbConn env
            isUser <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Users WHERE UserName = ? );")
                [uName]
            unless (maybe False fromOnly $ listToMaybe isUser) (throwError UserNOTExists)
            isPass <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Users WHERE UserName = ? AND Pass = md5(?));")
                (uName,uPass)
            unless (maybe False fromOnly $ listToMaybe isPass) (throwError WrongPass)
            _ <- execWithExcept pool
                (Query "UPDATE Users SET Pass = md5(?), Token = md5(?) WHERE UserName = ? ;")
                (uNewPass,makeHash (uName <> uNewPass),uName)
            u <- queryWithExcept pool
                (Query "SELECT * FROM Users WHERE UserName = ? ;")
                [uName]
            liftIO $ Logger.info (Logger.lConfig env) $ BS.toString $
                "Change password for user: " <> uName
            return $ A.toJSON (u :: [User])
        _ -> throwError NotFound
