{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module DataBase.Users where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types


import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Aeson as A
import qualified Data.ByteString.UTF8 as BS
import           Data.Text ( Text )
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time
import           Data.Maybe
import           Crypto.BCrypt

import           DataBase
import           Exceptions
import           Logger

data User = User
    { uid :: Int
    , firstName :: String
    , lastName :: String
    , avatar :: String
    , userName :: String
    , upass :: String
    , regDate :: Day
    , adm :: Bool
    , token :: String
    } deriving (Show,Eq)
instance A.ToJSON User where
    toJSON user = A.object
        [ "id"         A..= uid user
        , "name"       A..= userName user
        , "first_name" A..= firstName user
        , "last_name"  A..= lastName user
        , "avatar"     A..= avatar user
        , "reg_date"   A..= regDate user
        , "token"      A..= token user
        ]
instance FromRow User where
    fromRow = User
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field

emptyUser :: User
emptyUser = User
    { uid = 0
    , firstName = ""
    , lastName = ""
    , avatar = ""
    , userName = ""
    , upass = ""
    , regDate = ModifiedJulianDay 1
    , adm = False
    , token = ""
    }

parseUser :: [( BS.ByteString , Maybe BS.ByteString )] -> User
parseUser = foldr func emptyUser where
    func (pn,pe) user = case pn of
        "id" -> user {uid = read $ BS.toString $ fromMaybe ("0" :: BS.ByteString) pe}
        "last_name" -> user {lastName = BS.toString $ fromMaybe "" pe}
        "first_name" -> user {firstName = BS.toString $ fromMaybe "" pe}
        "name" -> user {userName = BS.toString $ fromMaybe "" pe}
        "avatar" -> user {avatar = BS.toString $ fromMaybe "" pe}
        "pass" -> user {upass = BS.toString $ makeHash $ fromMaybe "" pe}
        "token" -> user {token = BS.toString $ fromMaybe "" pe}
        _ -> user

makeHash :: BS.ByteString -> BS.ByteString
makeHash bs = fromMaybe "" $ hashPassword bs $
    fromMaybe "" $ genSalt "$2b$" 6 "qsfvkpkvtnhtefhk"

valUser :: User -> String
valUser u = "'" <> firstName u
    <> "','" <> lastName u <> "'"
    <> ",'" <> avatar u <> "'"
    <> ",'" <> userName u <> "'"
    <> ",md5('" <> upass u <> "')"
    <> ",md5('" <> (BS.toString . makeHash . BS.fromString) (userName u <> upass u) <> "')"

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
    oldUser <- liftIO $ queryDB pool $ (Query . BS.fromString) $
                "SELECT EXISTS (SELECT id FROM Users WHERE UserName = '"
                <> userName user
                <> "')"
    if fromOnly (head oldUser) then throwError ObjectExists else (do
        let us = user {token = (BS.toString . makeHash . BS.fromString) $
            userName user <> upass user}
        _ <- liftIO $ execDB pool $ (Query . BS.fromString) $
            "INSERT INTO Users "
            <> "(FirstName, LastName, Avatar, UserName, Pass, Token, RegDate)"
            <> "VALUES ("
            <> valUser user <> ", NOW ());"
        u <- liftIO $ queryDB pool $ (Query . BS.fromString) $
            "SELECT * FROM Users WHERE id>0 "
            <> addFieldToQuery "UserName" (userName user)
            <> addFieldToQuery "FirstName" (firstName user)
            <> addFieldToQuery "LastName" (lastName user)
        liftIO $ Logger.info (Logger.lConfig env) $ "Add user: " <> userName user
        return $ A.toJSON (u :: [User]))

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
            user <- liftIO $ queryDB pool $ Query $
                    "SELECT EXISTS (SELECT id FROM Users WHERE token = '"
                    <> fromMaybe "" token <> "');"
            adm <- liftIO $ queryDB pool $ Query $
                        "SELECT Adm FROM Users WHERE token = '"
                        <> fromMaybe "" token <> "';"
            case adm of
                [] -> return $ Just ( fromOnly $ head user , False)
                _  -> return $ Just ( fromOnly $ head user , fromOnly $ head adm)

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
            u <- liftIO $ queryDB pool $ (Query . BS.fromString) $
                "SELECT * FROM Users WHERE id>0 "
                <> addFieldToQuery "UserName" (userName user)
                <> addFieldToQuery "FirstName" (firstName user)
                <> addFieldToQuery "LastName" (lastName user)
                <> " ORDER BY UserName"
                <> getLimitOffset param
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
                    _ <- liftIO $ execDB pool $ Query $
                        "DELETE FROM Users WHERE id = " <> uid
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
            isUser <- liftIO $ queryDB pool $ Query $
                "SELECT EXISTS (SELECT id FROM Users WHERE UserName = '" <> uName <> "');"
            unless (fromOnly $ head isUser) (throwError UserNOTExists)
            isPass <- liftIO $ queryDB pool $ Query $
                "SELECT EXISTS (SELECT id FROM Users WHERE UserName = '"
                <> uName <> "' AND Pass = md5('" <> uPass <> "'));"
            unless (fromOnly $ head isPass) (throwError WrongPass)
            _ <- liftIO $ execDB pool $ Query $
                "UPDATE Users SET "
                <> "Pass = md5('" <> uNewPass <> "') "
                <> ", Token = md5('" <> makeHash (uName <> uNewPass) <> "') "
                <> "WHERE UserName = '" <> uName <> "';"
            u <- liftIO $ queryDB pool $ Query $
                "SELECT * FROM Users WHERE UserName = '" <> uName <> "';"
            liftIO $ Logger.info (Logger.lConfig env) $ BS.toString $
                "Change password for user: " <> uName
            return $ A.toJSON (u :: [User])
        _ -> throwError NotFound
