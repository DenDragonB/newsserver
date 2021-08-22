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
import qualified Data.ByteString.Conversion       as BS
import qualified Data.ByteString.UTF8             as BS
import           Data.Maybe
import qualified Data.Text                        as T
import           Data.Time
import           GHC.Generics

import           DataBase
import           Exceptions
import           Logger
import           Prelude                          hiding (id)
-- import qualified GHC.TypeLits as T

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
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
userAdd param = do

    mFirstName <- parseParam "first_name" param
    mLastName <- parseParam "last_name" param
    mUserName <- parseParam "name" param
    mAvatar <- parseParam "avatar" param
    mPass <- parseParam "pass" param 

    let userName = fromMaybe "" (mUserName :: Maybe T.Text)
    let userPass = fromMaybe "" (mPass :: Maybe T.Text)

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
        ( mFirstName :: Maybe T.Text
        , mLastName :: Maybe T.Text
        , mAvatar :: Maybe T.Text
        , mUserName
        , mPass
        , (BS.toString . makeHash . BS.toByteString') (userName <> userPass))
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
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m (Maybe (Bool,Bool))
findToken param = do
    env <- ask
    mToken <- parseParam "token" param
    let queryToken = fromMaybe "" (mToken :: Maybe T.Text)
    liftIO $ Logger.debug (Logger.lConfig env) $
                        "Request from user with token: " <> T.unpack queryToken
    if isNothing mToken
        then return Nothing
        else do
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
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
userGet param = do
    queryToken <- findToken param
    if isNothing queryToken
        then throwError NotFound
        else do
            env <- ask
            mFirstName <- parseParam "first_name" param
            mLastName <- parseParam "last_name" param
            mUserName <- parseParam "name" param
            let pool = dbConn env
            u <- queryWithExcept pool
                (Query $ "SELECT id,UserName,FirstName,LastName,Avatar,RegDate FROM Users,searchData WHERE"
                    <> "(UserName = COALESCE (?,UserName))"
                    <> "AND (FirstName = COALESCE (?,FirstName))"
                    <> "AND (LastName = COALESCE (?,LastName))"
                    <> "ORDER BY UserName "
                    <> getLimitOffsetBS param <> ";")
                ( mUserName :: Maybe T.Text
                , mFirstName :: Maybe T.Text
                , mLastName :: Maybe T.Text)
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
            muid <- parseParam "id" param
            case muid :: Maybe Int of
                Nothing -> throwError WrongQueryParameter
                Just uid -> do
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
