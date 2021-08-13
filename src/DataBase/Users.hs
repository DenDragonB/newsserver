{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Users where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import           Crypto.BCrypt
import qualified Data.Aeson                         as A
import qualified Data.ByteString.Conversion         as BS
import qualified Data.ByteString.UTF8               as BS
import           Data.Maybe
import           Data.Text                          (Text)
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
    func (pn,pe) user = case pn of
        "id"         -> user {id = fromMaybe 0 (pe >>= BS.fromByteString)}
        "last_name"  -> user {last_name = BS.toString $ fromMaybe "" pe}
        "first_name" -> user {first_name = BS.toString $ fromMaybe "" pe}
        "name"       -> user {name = BS.toString $ fromMaybe "" pe}
        "avatar"     -> user {avatar = BS.toString $ fromMaybe "" pe}
        "pass"       -> user {upass = BS.toString $ makeHash $ fromMaybe "" pe}
        "token"      -> user {token = BS.toString $ fromMaybe "" pe}
        _            -> user

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
    let user = parseUser param
    env <- ask
    let pool = dbConn env
    oldUser <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Users WHERE UserName = ? );")
                [name user]
    when (maybe False fromOnly $ listToMaybe oldUser) (throwError ObjectExists)
    let us = user {token = (BS.toString . makeHash . BS.fromString) $
        name user <> upass user}
    _ <- execWithExcept pool
        (Query "INSERT INTO Users "
            <> "(FirstName, LastName, Avatar, UserName, Pass, Token, RegDate)"
            <> "VALUES (?,?,?,?,md5(?),md5(?),NOW());")
        (first_name user, last_name user, avatar user, name user, upass user,
            (BS.toString . makeHash . BS.fromString) (name user <> upass user))
    u <- queryWithExcept pool
        (Query "SELECT * FROM Users WHERE UserName = ? AND FirstName = ? AND LastName = ?;")
        (name user, first_name user, last_name user)
    liftIO $ Logger.info (Logger.lConfig env) $ "Add user: " <> name user
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
    let token = getParam "token" param
    liftIO $ Logger.debug (Logger.lConfig env) $
                        "Request from user with token: " <> show token
    if isNothing token
        then return Nothing
        else do
            let pool = dbConn env
            user <- queryWithExcept pool
                    (Query "SELECT EXISTS (SELECT id FROM Users WHERE token = ? );")
                    [fromMaybe "" token]
            adm <- queryWithExcept pool
                    (Query "SELECT Adm FROM Users WHERE token = ? ;")
                    [fromMaybe "" token]
            case adm of
                [] -> return $ Just ( maybe False fromOnly $ listToMaybe user , False)
                _  -> return $ Just ( maybe False fromOnly $ listToMaybe user , maybe False fromOnly $ listToMaybe adm)

userGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
userGet param = do
    token <- findToken param
    if isNothing token
        then throwError NotFound
        else do
            env <- ask
            let user = parseUser param
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
                (name user,first_name user,last_name user)
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
