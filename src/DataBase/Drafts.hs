{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Drafts where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           GHC.Generics                       (Generic)


import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import qualified Data.ByteString.UTF8               as BS
import           Data.Maybe                         (fromMaybe, isNothing,
                                                     listToMaybe)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Data.Time

import           DataBase
import qualified DataBase.Posts                     as DB
import qualified DataBase.Users
import           Exceptions
import           Logger
import           Prelude                            hiding (id)

data Draft = Draft
    { id          :: Int
    , post_id     :: Int
    , header      :: Text
    , reg_date    :: Day
    , autor_id    :: Int
    , category_id :: Int
    , tags_id     :: PGArray Int
    , content     :: Text
    , main_photo  :: Text
    , photos      :: PGArray Text
    } deriving (Show,Eq,Generic)
instance A.ToJSON Draft
instance FromRow Draft

draftAdd ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
draftAdd param = do
    env <- ask
    let mtoken = getParam "token" param
    case mtoken of
        Just token -> do
            let pool = dbConn env
            authors <- queryWithExcept pool
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ? );")
                [token]
            case (authors :: [Only Int]) of
                [author] -> do
                    let mheader = getParam "header" param
                    let mcat = getParam "category_id" param
                    case sequence [mheader,mcat] of
                        Just [header,cat] -> do
                            let tags = fromMaybe "[]" $ getParam "tags_id" param
                            let cont = fromMaybe "" $ getParam "content" param
                            let mph = fromMaybe "" $ getParam "main_photo" param
                            let phs = fromMaybe "[]" $ getParam "photos" param
                            dids <- queryWithExcept pool
                                (Query "INSERT INTO Drafts "
                                    <> "(Header,RegDate,News,Author,Category,Tags,Content,MainPhoto,Photos)"
                                    <> " VALUES ( ?,NOW(),0,?,?, ARRAY ?,?,?, ARRAY ?) RETURNING Id;")
                                (header, fromOnly author,
                                cat,tags,cont,mph,phs)
                            liftIO $ Logger.info (Logger.lConfig env) $
                                "Add Draft: " <> BS.toString header
                            draft <- queryWithExcept pool
                                (Query "SELECT * FROM Drafts WHERE Id = ? ;")
                                $ fromOnly <$> (dids :: [Only Int])
                            return $ A.toJSON (draft :: [Draft])
                        _ -> throwError WrongQueryParameter
                _ -> throwError AuthorNOTExists
        _ -> throwError NotFound

draftEdit ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
draftEdit param = do
    env <- ask
    let mtoken = getParam "token" param
    let mdid = getParam "id" param
    case sequence [mtoken,mdid] of
        Just [token,did] -> do
            let pool = dbConn env
            authors <- queryWithExcept pool
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ?);")
                [token]
            when (null authors) (throwError AuthorNOTExists)
            let author = fromMaybe 0 $ listToMaybe $ fromOnly <$> (authors :: [Only Int])
            isDraft <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT id FROM Drafts WHERE Id = ? AND Author = ?);")
                (did,author)
            unless (maybe False fromOnly $ listToMaybe isDraft) (throwError ObjectNOTExists)
            let header = getParam "header" param
            let cat = getParam "category_id" param
            let tags = makeArray <$> getParam "tags_id" param
            let cont = getParam "content" param
            let mph = getParam "main_photo" param
            let phs = makeArray <$> getParam "photos" param
            _ <- execWithExcept pool
                (Query "UPDATE Drafts SET "
                    <> "Header = COALESCE (?, Header ),"
                    <> "Category = COALESCE (?, Category ),"
                    <> "Tags = COALESCE (?, Tags ),"
                    <> "Content = COALESCE (?, Content ),"
                    <> "MainPhoto = COALESCE (?, MainPhoto ),"
                    <> "Photos = COALESCE (?, Photos )"
                    <> "WHERE Id = ?;")
                (header,cat,tags,cont,mph,phs,did)
            liftIO $ Logger.info (Logger.lConfig env) $
                "Edit Draft id: " <> BS.toString did
            draft <- queryWithExcept pool
                (Query "SELECT * FROM Drafts WHERE Id = ? ;")
                [did]
            return $ A.toJSON (draft :: [Draft])
        _ -> throwError NotFound

draftGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
draftGet param = do
    env <- ask
    let mtoken = getParam "token" param
    let mdid = getParam "id" param
    case sequence [mtoken,mdid] of
        Just [token,did] -> do
            let pool = dbConn env
            authors <- queryWithExcept pool
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ?);")
                [token]
            when (null authors) (throwError AuthorNOTExists)
            let author = fromMaybe 0 $ listToMaybe $ fromOnly <$> (authors :: [Only Int])
            draft <- queryWithExcept pool
                (Query $ "SELECT * FROM Drafts WHERE Id = ? AND Author = ? "
                    <> "ORDER BY RegDate "
                    <> getLimitOffsetBS param <> ";")
                (did,author)
            when (null draft) (throwError ObjectNOTExists)
            return $ A.toJSON (draft :: [Draft])
        _ -> throwError NotFound

draftDelete ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
draftDelete param = do
    env <- ask
    let mtoken = getParam "token" param
    let mdid = getParam "id" param
    case sequence [mtoken,mdid] of
        Just [token,did] -> do
            let pool = dbConn env
            authors <- queryWithExcept pool
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ?);")
                [token]
            when (null authors) (throwError AuthorNOTExists)
            let author = fromMaybe 0 $ listToMaybe $ fromOnly <$> (authors :: [Only Int])
            isDraft <- queryWithExcept pool
                (Query "SELECT EXISTS (SELECT Id FROM Drafts WHERE Id = ? AND Author = ? );")
                (did,author)
            unless (maybe False fromOnly $ listToMaybe isDraft) (throwError ObjectNOTExists)
            _ <- liftIO $ execDBsafe pool
                (Query "DELETE FROM Drafts WHERE Id = ? AND Author = ? ;")
                (did,author)
            liftIO $ Logger.info (Logger.lConfig env) $
                "Delete Draft id: " <> BS.toString did
            return $ A.String $ decodeUtf8 $ "Draft with id " <> did <> " deleted"
        _ -> throwError NotFound

draftPublish ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
draftPublish param = do
    env <- ask
    let mtoken = getParam "token" param
    let mdid = getParam "id" param
    case sequence [mtoken,mdid] of
        Just [token,did] -> do
            let pool = dbConn env
            authors <- queryWithExcept pool
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ?);")
                [token]
            when (null authors) (throwError AuthorNOTExists)
            let author = fromMaybe 0 $ listToMaybe $ fromOnly <$> (authors :: [Only Int])
            drafts <- queryWithExcept pool
                (Query "SELECT * FROM Drafts WHERE Id = ? AND Author = ? ;")
                (did,author)
            when (null drafts) (throwError ObjectNOTExists)

            let draft = fromMaybe emptyDraft $ listToMaybe (drafts :: [Draft])
            news <- queryWithExcept pool
                (Query "SELECT Id FROM News WHERE Id = ? ;")
                [post_id draft]
            if null news
                then do
                    nids <- queryWithExcept pool
                        (Query "INSERT INTO News "
                            <> "(Header,RegDate,Author,Category,Tags,Content,MainPhoto,Photos)"
                            <> "(SELECT Header,RegDate,Author,Category,Tags,Content,MainPhoto,Photos "
                            <> "FROM Drafts WHERE Id = ? ) RETURNING Id;")
                        [did]
                    let nid = fromMaybe 0 $ listToMaybe $ fromOnly <$> (nids :: [Only Int])
                    _ <- execWithExcept pool
                        (Query "UPDATE Drafts SET News = ? WHERE Id = ? ;")
                        (nid,did)
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Publish News id: " <> show nid
                    DB.postGet [("token",Just token),("id",(Just . BS.fromString . show) nid)]
                else do
                    let nid = fromMaybe 0 $ listToMaybe $ fromOnly <$> (news :: [Only Int])
                    _ <- execWithExcept pool
                        (Query "UPDATE News SET "
                            <> "(Header,RegDate,Author,Category,Tags,Content,MainPhoto,Photos) = "
                            <> "(SELECT Header,RegDate,Author,Category,Tags,Content,MainPhoto,Photos "
                            <> "FROM Drafts WHERE Id = ?) WHERE Id = ? ;")
                        (did,nid)
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Publish News id: " <> show nid
                    DB.postGet [("token",Just token),("id",(Just . BS.fromString . show) nid)]
        _ -> throwError NotFound

emptyDraft :: Draft
emptyDraft = Draft
    { id       = 0
    , post_id    = 0
    , header    = ""
    , reg_date  = ModifiedJulianDay 0
    , autor_id   = 0
    , category_id     = 0
    , tags_id      = PGArray [0]
    , content   = ""
    , main_photo =""
    , photos    = PGArray [""]
    }
