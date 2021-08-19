{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DataBase.Drafts where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           GHC.Generics                       (Generic)


import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import qualified Data.ByteString.Conversion         as BS
import qualified Data.ByteString.UTF8               as BS
import           Data.Maybe                         (fromMaybe, isNothing,
                                                     listToMaybe)
import           Data.Text                          (Text, pack, unpack)
import           Data.Time

import           DataBase
import qualified DataBase.Posts                     as DB
import           Exceptions
import           Logger
import           Prelude                            hiding (id)

data Draft = Draft
    { dbid          :: Int
    , dbpost_id     :: Int
    , dbheader      :: Text
    , dbreg_date    :: Day
    , dbautor_id    :: Int
    , dbcategory_id :: Int
    , dbtags_id     :: PGArray Int
    , dbcontent     :: Text
    , dbmain_photo  :: Text
    , dbphotos      :: PGArray Text
    } deriving (Show,Eq,Generic)
instance FromRow Draft

data DraftJSON = DraftJSON
    { id          :: Int
    , post_id     :: Int
    , header      :: Text
    , reg_date    :: Day
    , autor_id    :: Int
    , category_id :: Int
    , tags_id     :: [Int]
    , content     :: Text
    , main_photo  :: Text
    , photos      :: [Text]
    } deriving (Show,Eq,Generic)
instance A.ToJSON DraftJSON

draftToJSON :: Draft -> DraftJSON
draftToJSON Draft {..} = DraftJSON
    dbid
    dbpost_id
    dbheader
    dbreg_date
    dbautor_id
    dbcategory_id
    (fromPGArray dbtags_id)
    dbcontent
    dbmain_photo
    (fromPGArray dbphotos)

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
    mtoken <- parseParam "token" param
    case mtoken :: Maybe Text of
        Just token -> do
            let pool = dbConn env
            authors <- queryWithExcept pool
                (Query "SELECT id FROM Authors WHERE UserId = "
                    <> "(SELECT Id FROM Users WHERE Token = ? );")
                [token]
            case (authors :: [Only Int]) of
                [author] -> do
                    mheader <- parseParam "header" param
                    mcat <- parseParam "category_id" param
                    when (isNothing mheader || isNothing mcat) $ throwError WrongQueryParameter
                    let queryHeader = fromMaybe "" (mheader :: Maybe Text)
                    let cat = fromMaybe 0 (mcat :: Maybe Int)
                    mtags <- parseParamDelBracket "tags_id" param
                    mcont <- parseParam "content" param
                    mmph <- parseParam "main_photo" param
                    mphs <- parseParamDelBracket "photos" param
                    let tags = maybe [] BS.fromList (mtags :: Maybe (BS.List Int))
                    let cont = fromMaybe "" (mcont :: Maybe Text)
                    let mph = fromMaybe "" (mmph :: Maybe Text)
                    let phs = maybe [] BS.fromList (mphs :: Maybe (BS.List Text))
                    dids <- queryWithExcept pool
                        (Query "INSERT INTO Drafts "
                            <> "(Header,RegDate,News,Author,Category,Tags,Content,MainPhoto,Photos)"
                            <> " VALUES ( ?,NOW(),0,?,?, ?,?,?, ?) RETURNING Id;")
                        (queryHeader, fromOnly author,
                            cat,PGArray tags,cont,mph, PGArray phs)
                    liftIO $ Logger.info (Logger.lConfig env) $
                        "Add Draft: " <> unpack queryHeader
                    draft <- queryWithExcept pool
                        (Query "SELECT * FROM Drafts WHERE Id = ? ;")
                        $ fromOnly <$> (dids :: [Only Int])
                    return $ A.toJSON $ draftToJSON <$> (draft :: [Draft])
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
    mtoken <- parseParam "token" param
    mdid <- parseParam "id" param
    when (isNothing mtoken || isNothing mdid) $ throwError NotFound
    let token = fromMaybe "" (mtoken :: Maybe Text)
    let did = fromMaybe 0 (mdid :: Maybe Int)

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

    queryHeader <- parseParam "header" param
    cat <- parseParam "category_id" param
    tags <- parseParamDelBracket "tags_id" param
    cont <- parseParam "content" param
    mph <- parseParam "main_photo" param
    phs <- parseParamDelBracket "photos" param

    _ <- execWithExcept pool
        (Query "UPDATE Drafts SET "
            <> "Header = COALESCE (?, Header ),"
            <> "Category = COALESCE (?, Category ),"
            <> "Tags = COALESCE (?, Tags ),"
            <> "Content = COALESCE (?, Content ),"
            <> "MainPhoto = COALESCE (?, MainPhoto ),"
            <> "Photos = COALESCE (?, Photos )"
            <> "WHERE Id = ?;")
        ( queryHeader :: Maybe Text
        , cat :: Maybe Int
        , PGArray . BS.fromList <$> (tags :: Maybe (BS.List Int))
        , cont :: Maybe Text
        , mph :: Maybe Text
        , PGArray . BS.fromList <$> (phs:: Maybe (BS.List Text))
        , did)
    liftIO $ Logger.info (Logger.lConfig env) $
        "Edit Draft id: " <> show did
    draft <- queryWithExcept pool
        (Query "SELECT * FROM Drafts WHERE Id = ? ;")
        [did]
    return $ A.toJSON $ draftToJSON <$> (draft :: [Draft])

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
    mtoken <- parseParam "token" param
    mdid <- parseParam "id" param
    when (isNothing mtoken || isNothing mdid) $ throwError NotFound
    let token = fromMaybe "" (mtoken :: Maybe Text)
    let did = fromMaybe 0 (mdid :: Maybe Int)

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
    return $ A.toJSON $ draftToJSON <$> (draft :: [Draft])

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
    mtoken <- parseParam "token" param
    mdid <- parseParam "id" param
    when (isNothing mtoken || isNothing mdid) $ throwError NotFound
    let token = fromMaybe "" (mtoken :: Maybe Text)
    let did = fromMaybe 0 (mdid :: Maybe Int)

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
        "Delete Draft id: " <> show did
    return $ A.String $ pack $ "Draft with id " <> show did <> " deleted"

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
    mtoken <- parseParam "token" param
    mdid <- parseParam "id" param
    when (isNothing mtoken || isNothing mdid) $ throwError NotFound
    let token = fromMaybe "" (mtoken :: Maybe Text)
    let did = fromMaybe 0 (mdid :: Maybe Int)

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
        [dbpost_id draft]
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
            DB.postGet [("token", (Just . BS.toByteString') token),("id",(Just . BS.toByteString') nid)]
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
            DB.postGet [("token",(Just . BS.toByteString') token),("id",(Just . BS.toByteString') nid)]

emptyDraft :: Draft
emptyDraft = Draft
    { dbid       = 0
    , dbpost_id    = 0
    , dbheader    = ""
    , dbreg_date  = ModifiedJulianDay 0
    , dbautor_id   = 0
    , dbcategory_id     = 0
    , dbtags_id      = PGArray [0]
    , dbcontent   = ""
    , dbmain_photo =""
    , dbphotos    = PGArray [""]
    }
