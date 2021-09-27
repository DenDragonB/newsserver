{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DataBase.Posts where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           GHC.Generics                       (Generic)


import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import qualified Data.ByteString.UTF8               as BS
import           Data.Function
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import           Data.Time

import           DataBase

import           Exceptions
import           Logger
import           Prelude                            hiding (id)

data News = News
    { dbid         :: Int
    , dbheader     :: Text
    , dbreg_date   :: Day
    , dbauthor     :: Text
    , dbcategory   :: Text
    , dbtags       :: PGArray Text
    , dbcontent    :: Text
    , dbmain_photo :: Text
    , dbphotos     :: PGArray Text
    } deriving (Show,Eq,Generic)
instance FromRow News

data NewsJSON = NewsJSON
    { id         :: Int
    , header     :: Text
    , reg_date   :: Day
    , author     :: Text
    , category   :: Text
    , tags       :: [Text]
    , content    :: Text
    , main_photo :: Text
    , photos     :: [Text]
    } deriving (Show,Eq,Generic)
instance A.ToJSON NewsJSON

newsToJSON :: News -> NewsJSON
newsToJSON News {..} = NewsJSON
    dbid
    dbheader
    dbreg_date
    dbauthor
    dbcategory
    (fromPGArray dbtags)
    dbcontent
    dbmain_photo
    (fromPGArray dbphotos)

postGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( String , Maybe String )]
    -> m A.Value
postGet param = do
    let nsort = fromMaybe "" $ getMaybeParam "sort_by" param
    nid <- parseMaybeParam "id" param
    ndate <- parseMaybeParam "created_at" param
    ndateLT <- parseMaybeParam "created_at__lt" param
    ndateGT <- parseMaybeParam "created_at__gt" param
    let nauthor = getMaybeParam "author" param
    ntag <- parseMaybeParam "tag" param
    ntagAll <- parseMaybeParamList "tags__all" param
    ntagIn <- parseMaybeParamList "tags__in" param
    let nheader = getMaybeParam "header" param
    let ncont = getMaybeParam "content" param
    let nsearch = getMaybeParam "search" param
    news <- queryWithExcept
        (Query $ "WITH filters AS (SELECT"
            <> " ?::INT AS news_id,"
            <> " ?::DATE AS ndate,"
            <> " ?::DATE AS ndate_LT,"
            <> " ?::DATE AS ndate_GT,"
            <> " ? AS nauthor,"
            <> " ?::INT AS ntag,"
            <> " ?::INT[] AS ntag_ALL,"
            <> " ?::INT[] AS ntag_IN,"
            <> " ? AS nheader,"
            <> " ? AS ncontent,"
            <> " lower(?) AS substring)"
            <> "SELECT n.Id, n.Header, n.RegDate, u.UserName, c.CatName, array_agg(t.tag) as tags, "
            <> "n.Content, n.MainPhoto, n.Photos "
            <> "FROM filters, News n"
            -- Add Name of Author
            <> " LEFT OUTER JOIN (Authors a JOIN Users u on a.userid = u.id)"
            <> " ON n.Author = a.id "
            -- Add name of Category
            <> " LEFT OUTER JOIN Categories c ON n.Category = c.id "
            -- Add names of Tags
            <> "LEFT OUTER JOIN newstotags nt ON nt.newsid=n.id "
            <> "LEFT JOIN Tags t ON t.id = nt.tagid "
            <> "WHERE "
            -- Add selection
            <> "(filters.news_id IS NULL OR n.id = filters.news_id) "
            <> "AND (filters.ndate IS NULL OR n.RegDate = filters.ndate) "
            <> "AND (filters.ndate_LT IS NULL OR n.RegDate < filters.ndate_LT) "
            <> "AND (filters.ndate_GT IS NULL OR n.RegDate > filters.ndate_GT) "
            <> "AND (filters.nauthor IS NULL OR strpos(u.UserName,filters.nauthor)>0) "
            <> "AND (filters.ntag IS NULL OR filters.ntag IN (SELECT tagid FROM newstotags WHERE newsid =n.Id)) "
            <> "AND (filters.ntag_ALL IS NULL OR "
            <> "     filters.ntag_ALL <@ (SELECT array_agg(tagid) FROM newstotags WHERE newsid=n.Id)) "
            <> "AND (filters.ntag_IN IS NULL OR "
            <> "     filters.ntag_IN && (SELECT array_agg(tagid) FROM newstotags WHERE newsid=n.Id)) "
            <> "AND (filters.nheader IS NULL OR strpos(n.Header,filters.nheader) > 0) "
            <> "AND (filters.ncontent IS NULL OR strpos(n.Content,filters.ncontent) > 0) "
            -- Add search
            <> "AND (filters.substring IS NULL OR "
            <> "     strpos (lower(n.Content),filters.substring) > 0 OR "
            <> "     strpos (lower(n.Header),filters.substring) > 0 OR "
            <> "     strpos (lower(u.UserName),filters.substring) > 0 OR "
            <> "     strpos (lower(t.tag),filters.substring) > 0 OR "
            <> "     strpos (lower(c.CatName),filters.substring) > 0) "
            -- group elements to correct work array_agg(tag)
            <> "GROUP BY n.Id, n.Header, n.RegDate, u.UserName, c.CatName, "
            <> "n.Content, n.MainPhoto, n.Photos "
            -- Add sorting
            <> addSortBy nsort
            -- Add pagination
            <> getLimitOffsetBS param
            <> ";")
        ( nid :: Maybe Int
        , ndate :: Maybe Day
        , ndateLT :: Maybe Day
        , ndateGT :: Maybe Day
        , nauthor
        , ntag :: Maybe Int
        , PGArray <$> (ntagAll :: Maybe [Int])
        , PGArray <$> (ntagIn :: Maybe [Int])
        , nheader
        , ncont
        , nsearch)
    return $ A.toJSON (newsToJSON <$> news)

addSortBy :: String -> BS.ByteString
addSortBy val = case val of
    "author"   -> " ORDER BY u.UserName "
    "date"     -> " ORDER BY n.RegDate "
    "category" -> " ORDER BY c.CatName "
    "photos"   -> " ORDER BY array_length (n.Photos,1) "
    _          -> ""
