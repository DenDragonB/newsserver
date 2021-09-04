{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DataBase.Users where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import           Crypto.BCrypt
import qualified Data.Aeson                       as A
import qualified Data.ByteString.UTF8             as BS
import           Data.Function                    ((&))
import           Data.Maybe
import qualified Data.Text                        as T
import           Data.Time
import           GHC.Generics

import           DataBase
import           Exceptions
import           Logger
import           Prelude                          hiding (id)

data User = User
    { id         :: Int
    , first_name :: String
    , last_name  :: String
    , avatar     :: String
    , name       :: String
    , upass      :: Maybe String
    , reg_date   :: Day
    , adm        :: Maybe Bool
    , token      :: Maybe String
    } deriving (Show,Eq,Generic)
instance A.ToJSON User
instance FromRow User

data UserToGet = UserToGet
    { id         :: Int
    , first_name :: String
    , last_name  :: String
    , avatar     :: String
    , name       :: String
    , reg_date   :: Day
    } deriving (Show,Eq,Generic)
instance A.ToJSON UserToGet
instance FromRow UserToGet

makeHash :: BS.ByteString -> BS.ByteString
makeHash bs = fromMaybe "" $ hashPassword bs $
    fromMaybe "" $ genSalt "$2b$" 6 "qsfvkpkvtnhtefhk"

userAdd ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
userAdd param = do

    userName <- getParamM "name" param
    when (null userName) (throwError $ WrongQueryParameter "name")

    userPass <- getParamM "pass" param
    when (null userPass) (throwError $ WrongQueryParameter "pass")

    let mFirstName = getMaybeParam "first_name" param
    let mLastName = getMaybeParam "last_name" param
    let mAvatar = getMaybeParam "avatar" param

    env <- ask
    let pool = dbConn env
    oldUser <- queryWithExcept pool
        (Query "SELECT EXISTS (SELECT id FROM Users WHERE UserName = ? );")
        [userName]
    when (maybe False fromOnly $ listToMaybe oldUser) (throwError ObjectExists)
    _ <- execWithExcept pool
        (Query "INSERT INTO Users "
        <> "(FirstName, LastName, Avatar, UserName, Pass, Token, RegDate)"
        <> "VALUES (?,?,?,?,md5(?),md5(?),NOW());")
        ( mFirstName
        , mLastName
        , mAvatar
        , userName
        , userPass
        , (BS.toString . makeHash . BS.fromString) (userName <> userPass))
    u <- queryWithExcept pool
        (Query "SELECT * FROM Users WHERE UserName = ? AND FirstName = ? AND LastName = ?;")
        (userName, mFirstName, mLastName)
    liftIO $ Logger.info (Logger.lConfig env) $ "Add user: " <> show u
    return $ A.toJSON (u :: [User])


findToken ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m (Maybe (Bool,Bool))
findToken param = do
    env <- ask
    let mToken = getMaybeParam "token" param
    case mToken of
        Nothing -> return Nothing
        Just queryToken -> do
            liftIO $ Logger.debug (Logger.lConfig env) $
                        "Request from user with token: " <> queryToken
            let pool = dbConn env
            userExist <- queryWithExcept pool
                    (Query "SELECT EXISTS (SELECT id FROM Users WHERE token = ? );")
                    [queryToken]
            admExist <- queryWithExcept pool
                    (Query "SELECT Adm FROM Users WHERE token = ? ;")
                    [queryToken]
            case admExist of
                [] -> return $ Just ( maybe False fromOnly $ listToMaybe userExist , False)
                _  -> return $ Just ( maybe False fromOnly $ listToMaybe userExist , maybe False fromOnly $ listToMaybe admExist)

userGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
userGet param = do
    queryToken <- findToken param
    if isNothing queryToken
        then throwError NotFound
        else do
            env <- ask
            let mFirstName = getMaybeParam "first_name" param
            let mLastName = getMaybeParam "last_name" param
            let mUserName = getMaybeParam "name" param
            let pool = dbConn env
            u <- queryWithExcept pool
                (Query $ "SELECT id,FirstName,LastName,Avatar,UserName,RegDate FROM Users WHERE"
                    <> "(UserName = COALESCE (?,UserName))"
                    <> "AND (FirstName = COALESCE (?,FirstName))"
                    <> "AND (LastName = COALESCE (?,LastName))"
                    <> "ORDER BY UserName "
                    <> getLimitOffsetBS param <> ";")
                ( mUserName
                , mFirstName
                , mLastName)
            return $ A.toJSON (u :: [UserToGet])

userDel ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
userDel param = do
    admin <- findToken param
    case admin of
        Just (_,True ) -> do
            uid <- parseParamM "id" param
            env <- ask
            let pool = dbConn env
            _ <- execWithExcept pool
                (Query "DELETE FROM Users WHERE id = ? ;")
                [uid :: Int]
            liftIO $ Logger.info (Logger.lConfig env) $
                "Delete user id: " <> show uid
            return $ A.String $ T.pack $ "User with id "<> show uid <>" deleted"
        _ -> throwError NotFound

userNewPass ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
userNewPass param = do
    env <- ask
    let mUserName = getMaybeParam "name" param
    uName <- mUserName & fromMaybeM NotFound
    when (null uName) (throwError $ WrongQueryParameter "name")

    let mPass = getMaybeParam "pass" param
    uPass <- mPass & fromMaybeM NotFound
    when (null uPass) (throwError $ WrongQueryParameter "pass")

    let mNewPass = getMaybeParam "new_pass" param
    uNewPass <- mNewPass & fromMaybeM NotFound
    when (null uNewPass) (throwError $ WrongQueryParameter "new_pass")

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
        ( uNewPass
        , (BS.toString . makeHash . BS.fromString) (uName <> uNewPass)
        , uName)
    u <- queryWithExcept pool
        (Query "SELECT * FROM Users WHERE UserName = ? ;")
        [uName]
    liftIO $ Logger.info (Logger.lConfig env) $
        "Change password for user: " <> uName
    return $ A.toJSON (u :: [User])

