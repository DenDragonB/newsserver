{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Authors where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import qualified Data.ByteString.UTF8               as BS
import           Data.Maybe                         (fromMaybe, listToMaybe)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           GHC.Generics

import           DataBase
import           DataBase.Users
import           Exceptions
import           Logger

data Author = Author
    { aid    :: Int
    , userId :: Int
    , about  :: Text
    } deriving (Show,Eq,Generic)
instance A.ToJSON Author where
    toJSON author = A.object
        [ "id"         A..= aid author
        , "user_id"    A..= userId author
        , "about"      A..= about author
        ]
instance FromRow Author

authorAdd ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
authorAdd param = do
    env <- ask
    admin <- findToken param
    liftIO $ Logger.debug (Logger.lConfig env) $
        "Just (User,Admin): " <> show admin
    case admin of
        Just (_, True ) -> do
            let muid = getParam "user_id" param
            let mabout = getParam "about" param
            case sequence [muid,mabout] of
                Just [uid,about] -> do
                    let pool = dbConn env
                    isUser <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Users WHERE id = ?);")
                        [uid]

                    unless (maybe False fromOnly $ listToMaybe isUser) (throwError UserNOTExists)
                    isAuthor <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Authors WHERE UserId = ?);")
                        [uid]
                    when (maybe False fromOnly $ listToMaybe isAuthor) (throwError ObjectExists)
                    _ <- execWithExcept pool
                        (Query "INSERT INTO Authors (UserId, About) VALUES (?,?);")
                        (uid , about )
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Add author with user id: " <> BS.toString uid
                    author <- queryWithExcept pool
                        (Query "SELECT * FROM Authors WHERE UserId = ? ;")
                        [uid]
                    return $ A.toJSON (author :: [Author])
                _ -> throwError WrongQueryParameter
        _ -> throwError NotFound

authorEdit ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
authorEdit param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let uid = getParam "user_id" param
            let about = getParam "about" param
            let maid = getParam "id" param
            case maid of
                Nothing -> throwError WrongQueryParameter
                Just aid -> do
                    let pool = dbConn env
                    isAuthor <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Authors WHERE Id = ?);")
                        [aid]
                    unless (maybe False fromOnly $ listToMaybe isAuthor) (throwError ObjectNOTExists)
                    _ <- execWithExcept pool
                        (Query "WITH "
                            <> "newData AS (SELECT CAST (? AS INT) as userid, CAST (? AS TEXT) as about),"
                            <> "oldData AS (SELECT id,userid,about from Authors WHERE id = ?)"
                            <> "UPDATE Authors SET "
                            <> "UserId = COALESCE ((SELECT userid from newData),(SELECT userid from oldData)),"
                            <> "About = COALESCE ((SELECT about from newData),(SELECT about from oldData))"
                            <> "WHERE Id = (SELECT id from oldData);")
                        (uid,about,aid)
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Edit author id: " <> BS.toString aid
                    author <- queryWithExcept pool
                        (Query "SELECT * FROM Authors WHERE Id = ? ;")
                        [aid]
                    return $ A.toJSON (author :: [Author])
        _ -> throwError NotFound

authorGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
authorGet param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let pool = dbConn env
            let uid = getParam "user_id" param
            let aid = getParam "id" param
            author <- queryWithExcept pool
                (Query $ "WITH searchData AS (SELECT "
                    <> " CAST (? as INT) AS sid "
                    <> ", CAST (? as INT) AS suserid ) "
                    <> "SELECT id,userid,about FROM authors,searchData WHERE"
                    <> "(suserid ISNULL OR suserid=userid)"
                    <> "AND (sid ISNULL OR sid=id)"
                    <> "ORDER BY id "
                    <> getLimitOffsetBS param <> ";")
                (aid,uid)
            return $ A.toJSON (author :: [Author])
        _ -> throwError NotFound

authorDelete ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
authorDelete param = do
    env <- ask
    admin <- findToken param
    case admin of
        Just (_, True ) -> do
            let maid = getParam "id" param
            case maid of
                Nothing -> throwError WrongQueryParameter
                Just aid -> do
                    let pool = dbConn env
                    isAuthor <- queryWithExcept pool
                        (Query "SELECT EXISTS (SELECT id FROM Authors WHERE Id = ?);")
                        [aid]
                    unless (maybe False fromOnly $ listToMaybe isAuthor) (throwError ObjectNOTExists)
                    _ <- execWithExcept pool
                        (Query "DELETE FROM Authors WHERE Id = ? ;")
                        [aid]
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Delete author id: " <> BS.toString aid
                    return $ A.String $ decodeUtf8 $ "Author with id "<>aid<>" deleted"
        _ -> throwError NotFound
