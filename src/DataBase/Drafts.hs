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

    draft <- selectDraftById (Just did)
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

    author <- selectAuthorIdByToken token
    when (isNothing author) (throwError AuthorNOTExists)
    
    did <- parseParamM "id" param
    draft <- selectDraftById (Just did)
    when (isNothing draft) (throwError ObjectNOTExists)

    if (dbautor_id <$> draft) /= author
        then throwError ObjectNOTExists
        else do
            deleteItemFromTableById Drafts did
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

    author <- selectAuthorIdByToken token
    when (isNothing author) (throwError AuthorNOTExists)

    did <- parseParamM "id" param
    mdraft <- selectDraftById (Just did)
    draft <- mdraft & fromMaybeM ObjectNOTExists

    maybeNewsId <- existItemByField News Id (dbpost_id draft)
    if maybeNewsId
       then updateNewsFromDraft (Just $ dbpost_id draft) did
       else do
            newsId <- insertNewsByDraftId did
            when (isNothing newsId) (throwError PostgreError)
            insertNewsToTagsUpdateDraft newsId did

    mnewdraft <- selectDraftById (Just did)
    newdraft <- mnewdraft & fromMaybeM PostgreError
    
    logConfig <- asks Logger.lConfig
    liftIO $ Logger.info logConfig $
        "Publish News id: " <> show (dbpost_id newdraft)

    return $ A.toJSON $ draftToJSON newdraft
