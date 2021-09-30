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
import           Data.Maybe                         (fromMaybe, isNothing,
                                                     listToMaybe)
import           Data.Text                          (Text, pack)
import           Data.Time

import           Data.Function
import           DataBase
import           DataBase.Postgres
import           DataBase.Types                     (Draft (..))
import           Exceptions
import           Logger
import           Prelude                            hiding (id)
import Control.Monad.Trans.Select (select)

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
    let mtoken = getMaybeParam "token" param
    token <- mtoken & fromMaybeM NotFound

    mauthor <- selectAuthorIdByToken token
    case mauthor of
        Just author -> do
            queryHeader <- getParamM "header" param

            cat <- parseParamM "category_id" param
            isCat <- existItemByField Categories Id (cat :: Int)
            unless isCat (throwError $ WrongQueryParameter "category_id")

            mtags <- parseMaybeParamList "tags_id" param
            let tags = fromMaybe [] (mtags :: Maybe [Int])
            bdtags <- selectTagsByIds tags
            unless (length tags==length bdtags)
                (throwError (WrongQueryParameter "tags_id"))

            let mcont = getMaybeParam "content" param
            let mmph = getMaybeParam "main_photo" param
            let mphs = getMaybeParamList "photos" param

            let cont = fromMaybe "" mcont
            let mph = fromMaybe "" mmph
            let phs = fromMaybe [] mphs
            
            newid <- insertDraftWithPrameters queryHeader author cat cont mph phs
            when (isNothing newid) (throwError Exceptions.PostgreError)

            updateDraftToTags newid tags

            logConfig <- asks Logger.lConfig
            liftIO $ Logger.info logConfig $
                        "Add Draft: " <> queryHeader

            draft <- selectDraftById newid
            return $ A.toJSON $ draftToJSON <$> draft
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
    let mtoken = getMaybeParam "token" param
    token <- mtoken & fromMaybeM NotFound

    author <- selectAuthorIdByToken token
    when (isNothing author) (throwError AuthorNOTExists)

    did <- parseParamM "id" param
    isDraft <- existDraftByIdAndAuthor did author
    unless isDraft (throwError ObjectNOTExists)

    cat <- parseMaybeParam "category_id" param
    isCat <- existItemByField Categories Id (cat :: Maybe Int)
    unless (isNothing cat || isCat) (throwError $ WrongQueryParameter "category_id")

    mtags <- parseMaybeParamList "tags_id" param
    let tags = fromMaybe [] (mtags :: Maybe [Int])
    bdtags <- selectTagsByIds tags
    unless (length tags==length bdtags)
        (throwError (WrongQueryParameter "tags_id"))

    let queryHeader = getMaybeParam "header" param
    let cont = getMaybeParam "content" param
    let mph = getMaybeParam "main_photo" param
    let phs = getMaybeParamList "photos" param

    updateDraftWithPrameters did queryHeader cat cont mph phs

    unless (isNothing mtags) $ updateDraftToTags (Just did) tags
    
    logConfig <- asks Logger.lConfig
    liftIO $ Logger.info logConfig $
                "Edit Draft id: " <> show did

    draft <- selectDraftById (Just did)
    return $ A.toJSON $ draftToJSON <$> draft

draftGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
draftGet param = do
    let mtoken = getMaybeParam "token" param
    token <- mtoken & fromMaybeM NotFound

    author <- selectAuthorIdByToken token
    when (isNothing author) (throwError AuthorNOTExists)

    did <- parseParamM "id" param

    draft <- selectDraftById did
    when (isNothing draft) (throwError ObjectNOTExists)

    if (dbautor_id <$> draft) == author
        then return $ A.toJSON $ draftToJSON <$> draft
        else throwError ObjectNOTExists

draftDelete ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
draftDelete param = do
    let mtoken = getMaybeParam "token" param
    token <- mtoken & fromMaybeM NotFound

    authors <- queryWithExcept
        (Query "SELECT id FROM Authors WHERE UserId = "
            <> "(SELECT Id FROM Users WHERE Token = ?);")
        [token]
    when (null authors) (throwError AuthorNOTExists)
    let author = fromMaybe 0 $ listToMaybe $ fromOnly <$> (authors :: [Only Int])
    did <- parseParamM "id" param
    isDraft <- queryWithExcept
        (Query "SELECT EXISTS (SELECT Id FROM Drafts WHERE Id = ? AND Author = ? );")
        (did :: Int,author)
    unless (maybe False fromOnly $ listToMaybe isDraft) (throwError ObjectNOTExists)
    _ <- execWithExcept
        (Query "DELETE FROM Drafts WHERE Id = ? AND Author = ? ;")
        (did,author)
    logConfig <- asks Logger.lConfig
    liftIO $ Logger.info logConfig $
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
    let mtoken = getMaybeParam "token" param
    token <- mtoken & fromMaybeM NotFound

    authors <- queryWithExcept
        (Query "SELECT id FROM Authors WHERE UserId = "
            <> "(SELECT Id FROM Users WHERE Token = ?);")
        [token]
    when (null authors) (throwError AuthorNOTExists)
    let author = fromMaybe 0 $ listToMaybe $ fromOnly <$> (authors :: [Only Int])
    did <- parseParamM "id" param
    drafts <- queryWithExcept
        (Query $ "SELECT d.Id,d.News,d.Header,d.RegDate,d.Author,d.Category, "
            <> "array_remove(array_agg(t.tagid),NULL),d.Content,d.MainPhoto,d.Photos "
            <> "FROM Drafts d "
            <> "LEFT OUTER JOIN draftstotags t ON t.draftid=d.id "
            <> "WHERE d.Id = ? AND d.Author = ? "
            <> "GROUP BY d.Id,d.News,d.Header,d.RegDate,d.Author,d.Category,d.Content,d.MainPhoto,d.Photos ")
        (did :: Int,author)
    when (null drafts) (throwError ObjectNOTExists)

    let draft = listToMaybe (drafts :: [Draft])
    news <- queryWithExcept
        (Query "SELECT Id FROM News WHERE Id = ? ;")
        [dbpost_id <$> draft]
    logConfig <- asks Logger.lConfig
    if null news
        then do
            nids <- queryWithExcept
                (Query "INSERT INTO News "
                    <> "(Header,RegDate,Author,Category,Content,MainPhoto,Photos)"
                    <> "(SELECT Header,RegDate,Author,Category,Content,MainPhoto,Photos "
                    <> "FROM Drafts WHERE Id = ? ) RETURNING Id;")
                [did]
            let nid = fromMaybe 0 $ listToMaybe $ fromOnly <$> (nids :: [Only Int])
            _ <- execWithExcept
                (Query "INSERT INTO newstotags (newsid,tagid) "
                    <> "SELECT ?,tagid FROM draftstotags WHERE draftid=?;"
                    <> "UPDATE Drafts SET News = ? WHERE Id = ? ;")
                (nid,did,nid,did)
            liftIO $ Logger.info logConfig $
                "Publish News id: " <> show nid
        else do
            let nid = fromMaybe 0 $ listToMaybe $ fromOnly <$> (news :: [Only Int])
            _ <- execWithExcept
                (Query "UPDATE News SET "
                    <> "(Header,RegDate,Author,Category,Content,MainPhoto,Photos) = "
                    <> "(SELECT Header,RegDate,Author,Category,Content,MainPhoto,Photos "
                    <> "FROM Drafts WHERE Id = ?) WHERE Id = ? ;"
                    <> "DELETE FROM newstotags WHERE newsid=?;"
                    <> "INSERT INTO newstotags (newsid,tagid) "
                    <> "SELECT ?,tagid FROM draftstotags WHERE draftid=?;" )
                (did,nid,nid,nid,did)
            liftIO $ Logger.info logConfig $
                "Publish News id: " <> show nid
    newdraft <- queryWithExcept
        (Query "SELECT d.Id,d.News,d.Header,d.RegDate,d.Author,d.Category, "
            <> "array_remove(array_agg(t.tagid),NULL),d.Content,d.MainPhoto,d.Photos "
            <> "FROM Drafts d "
            <> "LEFT OUTER JOIN draftstotags t ON t.draftid=d.id "
            <> "WHERE d.Id = ? "
            <> "GROUP BY d.Id,d.News,d.Header,d.RegDate,d.Author,d.Category,d.Content,d.MainPhoto,d.Photos")
        [did]
    return $ A.toJSON $ listToMaybe $ draftToJSON <$> (newdraft :: [Draft])
