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
import           Data.Maybe                         (fromMaybe, listToMaybe)
import           Data.Text                          (Text, pack)
import           Data.Time

import           Data.Function
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
    ) => [( String , Maybe String )]
    -> m A.Value
draftAdd param = do
    env <- ask
    let mtoken = getMaybeParam "token" param
    token <- mtoken & fromMaybeM NotFound

    let pool = dbConn env
    authors <- queryWithExcept pool
        (Query "SELECT id FROM Authors WHERE UserId = "
            <> "(SELECT Id FROM Users WHERE Token = ? );")
        [token]
    case (authors :: [Only Int]) of
        [author] -> do
            queryHeader <- getParamM "header" param
            cat <- parseParamM "category_id" param

            mtags <- parseMaybeParamList "tags_id" param
            let mcont = getMaybeParam "content" param
            let mmph = getMaybeParam "main_photo" param
            let mphs = getMaybeParamList "photos" param
            let tags = fromMaybe [] (mtags :: Maybe [Int])
            let cont = fromMaybe "" mcont
            let mph = fromMaybe "" mmph
            let phs = fromMaybe [] mphs
            dids <- queryWithExcept pool
                (Query "INSERT INTO Drafts "
                    <> "(Header,RegDate,News,Author,Category,Tags,Content,MainPhoto,Photos)"
                    <> " VALUES ( ?,NOW(),0,?,?, ?,?,?, ?) RETURNING Id;")
                (queryHeader, fromOnly author,
                    cat :: Int,PGArray tags,cont,mph, PGArray phs)
            liftIO $ Logger.info (Logger.lConfig env) $
                "Add Draft: " <> queryHeader
            draft <- queryWithExcept pool
                (Query "SELECT * FROM Drafts WHERE Id = ? ;")
                $ fromOnly <$> (dids :: [Only Int])
            return $ A.toJSON $ listToMaybe $ draftToJSON <$> (draft :: [Draft])
        _ -> throwError AuthorNOTExists

draftEdit ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
draftEdit param = do
    env <- ask

    let mtoken = getMaybeParam "token" param
    token <- mtoken & fromMaybeM NotFound

    let pool = dbConn env
    authors <- queryWithExcept pool
        (Query "SELECT id FROM Authors WHERE UserId = "
            <> "(SELECT Id FROM Users WHERE Token = ?);")
        [token]
    when (null authors) (throwError AuthorNOTExists)
    let author = fromMaybe 0 $ listToMaybe $ fromOnly <$> (authors :: [Only Int])

    did <- parseParamM "id" param
    isDraft <- queryWithExcept pool
        (Query "SELECT EXISTS (SELECT id FROM Drafts WHERE Id = ? AND Author = ?);")
        (did :: Int,author)
    unless (maybe False fromOnly $ listToMaybe isDraft) (throwError ObjectNOTExists)

    let queryHeader = getMaybeParam "header" param
    cat <- parseMaybeParam "category_id" param
    tags <- parseMaybeParamList "tags_id" param
    let cont = getMaybeParam "content" param
    let mph = getMaybeParam "main_photo" param
    let phs = getMaybeParamList "photos" param

    _ <- execWithExcept pool
        (Query "UPDATE Drafts SET "
            <> "Header = COALESCE (?, Header ),"
            <> "Category = COALESCE (?, Category ),"
            <> "Tags = COALESCE (?, Tags ),"
            <> "Content = COALESCE (?, Content ),"
            <> "MainPhoto = COALESCE (?, MainPhoto ),"
            <> "Photos = COALESCE (?, Photos )"
            <> "WHERE Id = ?;")
        ( queryHeader
        , cat :: Maybe Int
        , PGArray <$> (tags :: Maybe [Int])
        , cont
        , mph
        , PGArray <$> phs
        , did)
    liftIO $ Logger.info (Logger.lConfig env) $
        "Edit Draft id: " <> show did
    draft <- queryWithExcept pool
        (Query "SELECT * FROM Drafts WHERE Id = ? ;")
        [did]
    return $ A.toJSON $ listToMaybe $ draftToJSON <$> (draft :: [Draft])

draftGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
draftGet param = do
    env <- ask

    let mtoken = getMaybeParam "token" param
    token <- mtoken & fromMaybeM NotFound

    let pool = dbConn env
    authors <- queryWithExcept pool
        (Query "SELECT id FROM Authors WHERE UserId = "
            <> "(SELECT Id FROM Users WHERE Token = ?);")
        [token]
    when (null authors) (throwError AuthorNOTExists)
    let author = fromMaybe 0 $ listToMaybe $ fromOnly <$> (authors :: [Only Int])

    did <- parseParamM "id" param
    draft <- queryWithExcept pool
        (Query $ "SELECT * FROM Drafts WHERE Id = ? AND Author = ? "
            <> "ORDER BY RegDate "
            <> getLimitOffsetBS param <> ";")
        (did :: Int,author)
    when (null draft) (throwError ObjectNOTExists)
    return $ A.toJSON $ listToMaybe $ draftToJSON <$> (draft :: [Draft])

draftDelete ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
draftDelete param = do
    env <- ask

    let mtoken = getMaybeParam "token" param
    token <- mtoken & fromMaybeM NotFound

    let pool = dbConn env
    authors <- queryWithExcept pool
        (Query "SELECT id FROM Authors WHERE UserId = "
            <> "(SELECT Id FROM Users WHERE Token = ?);")
        [token]
    when (null authors) (throwError AuthorNOTExists)
    let author = fromMaybe 0 $ listToMaybe $ fromOnly <$> (authors :: [Only Int])
    did <- parseParamM "id" param
    isDraft <- queryWithExcept pool
        (Query "SELECT EXISTS (SELECT Id FROM Drafts WHERE Id = ? AND Author = ? );")
        (did :: Int,author)
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
    ) => [( String , Maybe String )]
    -> m A.Value
draftPublish param = do
    env <- ask

    let mtoken = getMaybeParam "token" param
    token <- mtoken & fromMaybeM NotFound

    let pool = dbConn env
    authors <- queryWithExcept pool
        (Query "SELECT id FROM Authors WHERE UserId = "
            <> "(SELECT Id FROM Users WHERE Token = ?);")
        [token]
    when (null authors) (throwError AuthorNOTExists)
    let author = fromMaybe 0 $ listToMaybe $ fromOnly <$> (authors :: [Only Int])
    did <- parseParamM "id" param
    drafts <- queryWithExcept pool
        (Query "SELECT * FROM Drafts WHERE Id = ? AND Author = ? ;")
        (did :: Int,author)
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
            DB.postGet [("token", mtoken),("id",(Just . show) nid)]
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
            DB.postGet [("token", mtoken),("id",(Just . show) nid)]

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
