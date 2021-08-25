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
    env <- ask
    let mtoken = getParam "token" param
    case mtoken of
        Just _ -> do
            let pool = dbConn env
            let nsort = fromMaybe "" $ getParam "sort_by" param
            nid <- parseParam "id" param
            ndate <- parseParam "created_at" param
            ndateLT <- parseParam "created_at__lt" param
            ndateGT <- parseParam "created_at__gt" param
            let nauthor = getParam "author" param
            ntag <- parseParamList "tag" param
            ntagAll <- parseParamList "tags__all" param
            ntagIn <- parseParamList "tags__in" param
            let nheader = getParam "header" param
            let ncont = getParam "content" param
            news <- queryWithExcept pool
                (Query $ "SELECT n.Id, n.Header, n.RegDate, u.UserName, c.CatName, array_agg(t.tag) as tags, "
                    <> "n.Content, n.MainPhoto, n.Photos  FROM News n"
                    -- Add Name of Author
                    <> " LEFT OUTER JOIN (Authors a JOIN Users u on a.userid = u.id)"
                    <> " ON n.Author = a.id "
                    -- Add name of Category
                    <> " LEFT OUTER JOIN Categories c ON n.Category = c.id "
                    -- Add names of Tags
                    <> "LEFT JOIN Tags t ON t.id = ANY(n.tags) "
                    <> " WHERE "
                    -- Add selection
                    <> "(n.id = COALESCE (?,n.id))"
                    <> "AND (n.RegDate = COALESCE (?,n.RegDate))"
                    <> "AND (n.RegDate < COALESCE (?,n.RegDate+1))"
                    <> "AND (n.RegDate > COALESCE (?,n.RegDate-1))"
                    <> "AND (strpos(u.UserName,COALESCE(?,u.UserName))>0 )"
                    <> "AND (Tags && COALESCE(?,Tags))"
                    <> "AND (COALESCE (?,Tags) <@ Tags)"
                    <> "AND (Tags && COALESCE (?,Tags))"
                    <> "AND (strpos(n.Header,COALESCE (?,n.Header)) > 0)"
                    <> "AND (strpos(n.Content,COALESCE (?,n.Content)) > 0)"
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
                , PGArray <$> (ntag :: Maybe [Int])
                , PGArray <$> (ntagAll :: Maybe [Int])
                , PGArray <$> (ntagIn :: Maybe [Int])
                , nheader
                , ncont )
            return $ A.toJSON (newsToJSON <$> news)
        _ -> throwError NotFound

addSortBy :: String -> BS.ByteString
addSortBy val = case val of
    "author"   -> " ORDER BY u.UserName "
    "date"     -> " ORDER BY n.RegDate "
    "category" -> " ORDER BY c.CatName "
    "photos"   -> " ORDER BY array_length (n.Photos,1) "
    _          -> ""
