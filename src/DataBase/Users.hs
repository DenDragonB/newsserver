{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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

    let mUserName = getParam "name" param
    userName <- mUserName & fromMaybeM WrongQueryParameter
    when (null userName) (throwError WrongQueryParameter)

    let mPass = getParam "pass" param
    userPass <- mPass & fromMaybeM WrongQueryParameter
    when (null userPass) (throwError WrongQueryParameter)

    let mFirstName = getParam "first_name" param
    let mLastName = getParam "last_name" param
    let mAvatar = getParam "avatar" param

    env <- ask
    let pool = dbConn env
    oldUser <- queryWithExcept pool
        (Query "SELECT EXISTS (SELECT id FROM Users WHERE UserName = ? );")
        [mUserName]
    when (maybe False fromOnly $ listToMaybe oldUser) (throwError ObjectExists)
    _ <- execWithExcept pool
        (Query "INSERT INTO Users "
        <> "(FirstName, LastName, Avatar, UserName, Pass, Token, RegDate)"
        <> "VALUES (?,?,?,?,md5(?),md5(?),NOW());")
        ( mFirstName
        , mLastName
        , mAvatar
        , mUserName
        , mPass
        , (BS.toString . makeHash . BS.fromString) (userName <> userPass))
    u <- queryWithExcept pool
        (Query "SELECT * FROM Users WHERE UserName = ? AND FirstName = ? AND LastName = ?;")
        (mUserName, mFirstName, mLastName)
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
    let mToken = getParam "token" param
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
            let mFirstName = getParam "first_name" param
            let mLastName = getParam "last_name" param
            let mUserName = getParam "name" param
            let pool = dbConn env
            u <- queryWithExcept pool
                (Query $ "SELECT id,UserName,FirstName,LastName,Avatar,RegDate FROM Users,searchData WHERE"
                    <> "(UserName = COALESCE (?,UserName))"
                    <> "AND (FirstName = COALESCE (?,FirstName))"
                    <> "AND (LastName = COALESCE (?,LastName))"
                    <> "ORDER BY UserName "
                    <> getLimitOffsetBS param <> ";")
                ( mUserName
                , mFirstName
                , mLastName)
            return $ A.toJSON (u :: [User])

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
            muid <- parseParam "id" param
            uid <- (muid :: Maybe Int) & fromMaybeM WrongQueryParameter
            env <- ask
            let pool = dbConn env
            _ <- execWithExcept pool
                (Query "DELETE FROM Users WHERE id = ? ;")
                [uid]
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
    let mUserName = getParam "name" param
    uName <- mUserName & fromMaybeM NotFound
    when (null uName) (throwError WrongQueryParameter)

    let mPass = getParam "pass" param
    uPass <- mPass & fromMaybeM NotFound
    when (null uPass) (throwError WrongQueryParameter)

    let mNewPass = getParam "new_pass" param
    uNewPass <- mNewPass & fromMaybeM NotFound
    when (null uNewPass) (throwError WrongQueryParameter)

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

