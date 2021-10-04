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
import           DataBase.Types
import           Exceptions
import           Logger
import           Prelude                          hiding (id)

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

    oldUser <- existItemByField Users UserName userName
    when oldUser (throwError ObjectExists)

    userPass <- getParamM "pass" param
    when (null userPass) (throwError $ WrongQueryParameter "pass")

    let mFirstName = getMaybeParam "first_name" param
    let mLastName = getMaybeParam "last_name" param
    let mAvatar = getMaybeParam "avatar" param

    u <- insertUserWithParameters userName mFirstName mLastName mAvatar
        userPass ((BS.toString . makeHash . BS.fromString) (userName <> userPass))

    logConfig <- asks Logger.lConfig
    liftIO $ Logger.info logConfig $ "Add user: " <> show u
    return $ A.toJSON u


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
            userExist <- existItemByField Users Token queryToken
            admExist <- selectAdmFromUsers queryToken
            return $ Just ( userExist , admExist)

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
            u <- selectUserWithParameters mUserName mFirstName mLastName param
            return $ A.toJSON u

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
            deleteItemFromTableById Users uid
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

    isUser <- existItemByField Users UserName uName
    unless isUser (throwError UserNOTExists)

    isPass <- checkPassCorrect uName uPass
    unless isPass (throwError WrongPass)

    u <- changePassToUser uName uNewPass
        ((BS.toString . makeHash . BS.fromString) (uName <> uNewPass))
    logConfig <- asks Logger.lConfig
    liftIO $ Logger.info logConfig $
        "Change password for user: " <> uName
    return $ A.toJSON u

