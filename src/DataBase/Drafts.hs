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
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Data.Time

import           DataBase
import qualified DataBase.Posts                     as DB
import           DataBase.Users
import           Exceptions
import           Logger

data Draft = Draft
    { did       :: Int
    , newsId    :: Int
    , header    :: Text
    , newsDate  :: Day
    , autorId   :: Int
    , catId     :: Int
    , tags      :: PGArray Int
    , content   :: Text
    , mainPhoto :: Text
    , photos    :: PGArray Text
    } deriving (Show,Eq)
instance A.ToJSON Draft where
    toJSON draft = A.object
        [ "id"          A..= did draft
        , "post_id"     A..= newsId draft
        , "header"      A..= header draft
        , "reg_date"    A..= newsDate draft
        , "author_id"   A..= autorId draft
        , "category_id" A..= catId draft
        , "tags_id"     A..= fromPGArray (tags draft)
        , "content"     A..= content draft
        , "main_photo"  A..= mainPhoto draft
        , "photos"      A..= fromPGArray (photos draft)
        ]
instance FromRow Draft where
    fromRow = Draft
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field

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
            authors <- liftIO $ queryDBsafe pool 
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ? );")
                [token]
            case (authors :: [Only Int]) of
                [] -> throwError AuthorNOTExists
                _ -> do
                    let mheader = getParam "header" param
                    let mcat = getParam "category_id" param
                    case sequence [mheader,mcat] of
                        Just [header,cat] -> do
                            let tags = fromMaybe "[]" $ getParam "tags_id" param
                            let cont = fromMaybe "" $ getParam "content" param
                            let mph = fromMaybe "" $ getParam "main_photo" param
                            let phs = fromMaybe "[]" $ getParam "photos" param
                            dids <- liftIO $ queryDBsafe pool 
                                (Query "INSERT INTO Drafts "
                                    <> "(Header,RegDate,News,Author,Category,Tags,Content,MainPhoto,Photos)"
                                    <> " VALUES ( ?,NOW(),0,?,?, ARRAY ?,?,?, ARRAY ?) RETURNING Id;")
                                (header,(BS.fromString . show . fromOnly . head) authors,
                                cat,tags,cont,mph,phs)
                            liftIO $ Logger.info (Logger.lConfig env) $
                                "Add Draft: " <> BS.toString header
                            draft <- liftIO $ queryDBsafe pool 
                                (Query "SELECT * FROM Drafts WHERE Id = ? ;")
                                [(show . fromOnly . head) (dids :: [Only Int])]
                            return $ A.toJSON (draft :: [Draft])
                        _ -> throwError WrongQueryParameter
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
            authors <- liftIO $ queryDBsafe pool 
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ?);")
                [token]
            when (null authors) (throwError AuthorNOTExists)
            let author = (BS.fromString . show . fromOnly . head) (authors :: [Only Int])
            isDraft <- liftIO $ queryDBsafe pool 
                (Query "SELECT EXISTS (SELECT id FROM Drafts WHERE Id = ? AND Author = ?);")
                (did,author)
            unless (fromOnly $ head isDraft) (throwError ObjectNOTExists)
            let header = fromMaybe "" $ getParam "header" param
            let cat = fromMaybe "" $ getParam "category_id" param
            let tags = fromMaybe "" $ getParam "tags_id" param
            let cont = fromMaybe "" $ getParam "content" param
            let mph = fromMaybe "" $ getParam "main_photo" param
            let phs = fromMaybe "" $ getParam "photos" param
            _ <- liftIO $ execDB pool $ Query $
                "UPDATE Drafts SET Id = " <> did
                <> addToUpdate "Header" header
                <> addToUpdateNum "Category" cat
                <> addToUpdateNumArray "Tags" tags
                <> addToUpdate "Content" cont
                <> addToUpdate "MainPhoto" mph
                <> addToUpdateNumArray "Photos" phs
                <> " WHERE Id = " <> did <> ";"
            liftIO $ Logger.info (Logger.lConfig env) $
                "Edit Draft id: " <> BS.toString did
            draft <- liftIO $ queryDBsafe pool
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
            authors <- liftIO $ queryDBsafe pool 
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ?);")
                [token]
            when (null authors) (throwError AuthorNOTExists)
            let author = (BS.fromString . show . fromOnly . head) (authors :: [Only Int])
            draft <- liftIO $ queryDBsafe pool 
                (Query "SELECT * FROM Drafts WHERE Id = ? AND Author = ? ;")
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
            authors <- liftIO $ queryDBsafe pool 
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ?);")
                [token]
            when (null authors) (throwError AuthorNOTExists)
            let author = (BS.fromString . show . fromOnly . head) (authors :: [Only Int])
            isDraft <- liftIO $ queryDBsafe pool 
                (Query "SELECT EXISTS (SELECT Id FROM Drafts WHERE Id = ? AND Author = ? );")
                (did,author)
            unless (fromOnly $ head isDraft) (throwError ObjectNOTExists)
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
            authors <- liftIO $ queryDBsafe pool 
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ?);"
                [token]
            when (null authors) (throwError AuthorNOTExists)
            let author = (BS.fromString . show . fromOnly . head) (authors :: [Only Int])
            drafts <- liftIO $ queryDBsafe pool
                (Query "SELECT * FROM Drafts WHERE Id = ? AND Author = ? ;")
                (did,author)
            when (null drafts) (throwError ObjectNOTExists)
            let draft = head (drafts :: [Draft])
            news <- liftIO $ queryDBsafe pool 
                (Query "SELECT Id FROM News WHERE Id = ? ;")
                [newsId draft]
            if null news
                then do
                    nids <- liftIO $ queryDBsafe pool 
                        (Query "INSERT INTO News "
                            <> "(Header,RegDate,Author,Category,Tags,Content,MainPhoto,Photos)"
                            <> "(SELECT Header,RegDate,Author,Category,Tags,Content,MainPhoto,Photos "
                            <> "FROM Drafts WHERE Id = ? ) RETURNING Id;"
                        [did]
                    let nid = (fromOnly . head) (nids :: [Only Int])
                    _ <- liftIO $ execDBsafe pool 
                        (Query "UPDATE Drafts SET News = ? WHERE Id = ? ;")
                        (nid,did)
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Publish News id: " <> show nid
                    DB.postGet [("token",Just token),("id",(Just . BS.fromString . show) nid)]
                else do
                    let nid = (fromOnly . head) (news :: [Only Int])
                    _ <- liftIO $ execDBsafe pool
                        (Query "UPDATE News SET "
                            <> "(Header,RegDate,Author,Category,Tags,Content,MainPhoto,Photos) = "
                            <> "(SELECT Header,RegDate,Author,Category,Tags,Content,MainPhoto,Photos "
                            <> "FROM Drafts WHERE Id = ?) WHERE Id = ? ;"
                        (did,nid)   
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Publish News id: " <> show nid
                    DB.postGet [("token",Just token),("id",(Just . BS.fromString . show) nid)]
        _ -> throwError NotFound
