{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

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
import           DataBase.Postgres
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

    oldUser <- existItemByField "Users" "UserName" userName
    when oldUser (throwError ObjectExists)

    _ <- execWithExcept
        (Query "INSERT INTO Users "
        <> "(FirstName, LastName, Avatar, UserName, Pass, Token, RegDate)"
        <> "VALUES (?,?,?,?,md5(?),md5(?),NOW());")
        ( mFirstName
        , mLastName
        , mAvatar
        , userName
        , userPass
        , (BS.toString . makeHash . BS.fromString) (userName <> userPass))
    u <- queryWithExcept
        (Query "SELECT * FROM Users WHERE UserName = ? AND FirstName = ? AND LastName = ?;")
        (userName, mFirstName, mLastName)
    logConfig <- asks Logger.lConfig
    liftIO $ Logger.info logConfig $ "Add user: " <> show u
    return $ A.toJSON $ listToMaybe (u :: [User])


findToken ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m (Maybe (Bool,Bool))
findToken param = do
    let mToken = getMaybeParam "token" param
    case mToken of
        Nothing -> return Nothing
        Just queryToken -> do
            logConfig <- asks Logger.lConfig
            liftIO $ Logger.debug logConfig $
                        "Request from user with token: " <> queryToken
            userExist <- existItemByField "Users" "token" queryToken
            admExist <- queryWithExcept
                    (Query "SELECT Adm FROM Users WHERE token = ? ;")
                    [queryToken]
            case admExist of
                [] -> return $ Just ( userExist , False)
                _  -> return $ Just ( userExist , maybe False fromOnly $ listToMaybe admExist)

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
            let mFirstName = getMaybeParam "first_name" param
            let mLastName = getMaybeParam "last_name" param
            let mUserName = getMaybeParam "name" param
            u <- queryWithExcept
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
            _ <- execWithExcept
                (Query "DELETE FROM Users WHERE id = ? ;")
                [uid :: Int]
            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
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
    let mUserName = getMaybeParam "name" param
    uName <- mUserName & fromMaybeM NotFound
    when (null uName) (throwError $ WrongQueryParameter "name")

    let mPass = getMaybeParam "pass" param
    uPass <- mPass & fromMaybeM NotFound
    when (null uPass) (throwError $ WrongQueryParameter "pass")

    let mNewPass = getMaybeParam "new_pass" param
    uNewPass <- mNewPass & fromMaybeM NotFound
    when (null uNewPass) (throwError $ WrongQueryParameter "new_pass")

    isUser <- existItemByField "Users" "UserName" uName
    unless isUser (throwError UserNOTExists)

    isPass <- queryWithExcept
        (Query "SELECT EXISTS (SELECT id FROM Users WHERE UserName = ? AND Pass = md5(?));")
        (uName,uPass)
    unless (maybe False fromOnly $ listToMaybe isPass) (throwError WrongPass)

    _ <- execWithExcept
        (Query "UPDATE Users SET Pass = md5(?), Token = md5(?) WHERE UserName = ? ;")
        ( uNewPass
        , (BS.toString . makeHash . BS.fromString) (uName <> uNewPass)
        , uName)
    u <- selectMaybeItemByField "Users" "UserName" uName
    logConfig <- asks Logger.lConfig
    liftIO $ Logger.info logConfig $
        "Change password for user: " <> uName
    return $ A.toJSON (u :: Maybe User)

