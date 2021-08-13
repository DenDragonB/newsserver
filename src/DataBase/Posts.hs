{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module DataBase.Posts where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           GHC.Generics                       (Generic)


import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson                         as A
import qualified Data.ByteString.UTF8               as BS
import           Data.Maybe                         (fromMaybe, isNothing)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Data.Time

import           DataBase
import           DataBase.Users
import           Exceptions
import           Logger

data News = News
    { id         :: Int
    , header     :: Text
    , reg_date   :: Day
    , author     :: Text
    , category   :: Text
    , tags       :: PGArray Text
    , content    :: Text
    , main_photo :: Text
    , photos     :: PGArray Text
    } deriving (Show,Eq,Generic)
instance A.ToJSON News
instance FromRow News

postGet ::
    ( MonadReader env m
    , HasDataBase env
    , HasLogger env
    , MonadError Errors m
    , MonadIO m
    ) => [( BS.ByteString , Maybe BS.ByteString )]
    -> m A.Value
postGet param = do
    env <- ask
    let mtoken = getParam "token" param
    case sequence [mtoken] of
        Just [token] -> do
            let pool = dbConn env
            let nsort = fromMaybe "" $ getParam "sort_by" param
            let nid = getParam "id" param
            let ndate = getParam "created_at" param
            let ndateLT = getParam "created_at__lt" param
            let ndateGT = getParam "created_at__gt" param
            let nauthor = getParam "author" param
            let ntag = getParam "tag" param
            let ntagAll = makeArray <$> getParam "tags__all" param
            let ntagIn = makeArray <$> getParam "tags__in" param
            let nheader = getParam "header" param
            let ncont = getParam "content" param
            news <- queryWithExcept pool
                (Query $ "WITH searchData AS (SELECT "
                    <> " CAST (? as INT) AS sid "
                    <> ", CAST (? as DATE) AS screateAt "
                    <> ", CAST (? as DATE) AS screateLT "
                    <> ", CAST (? as DATE) AS screateGT "
                    <> ", CAST (? as TEXT) AS sauthorName "
                    <> ", CAST (" <> (if isNothing ntag then "?" else "ARRAY [?]") <> " as INT[]) AS stagOne "
                    <> ", CAST (? as INT[]) AS stagsALL "
                    <> ", CAST (? as INT[]) AS stagsIN "
                    <> ", CAST (? as TEXT) AS sheader "
                    <> ", CAST (? as TEXT) AS scontent )"
                    <> "SELECT n.Id, n.Header, n.RegDate, u.UserName, c.CatName, array_agg(t.tag) as tags, "
                    <> "n.Content, n.MainPhoto, n.Photos  FROM searchData , News n"
                    -- Add Name of Author
                    <> " LEFT OUTER JOIN (Authors a JOIN Users u on a.userid = u.id)"
                    <> " ON n.Author = a.id "
                    -- Add name of Category
                    <> " LEFT OUTER JOIN Categories c ON n.Category = c.id "
                    -- Add names of Tags
                    <> "LEFT JOIN Tags t ON t.id = ANY(n.tags) "
                    <> " WHERE "
                    -- Add selection
                    <> "(sid ISNULL OR sid=n.id)"
                    <> "AND (screateAt ISNULL OR screateAt=n.RegDate)"
                    <> "AND (screateLT ISNULL OR screateLT>n.RegDate)"
                    <> "AND (screateGT ISNULL OR screateGT<n.RegDate)"
                    <> "AND (sauthorName ISNULL OR strpos(u.UserName,sauthorName)>0 )"
                    <> "AND (stagOne ISNULL OR Tags && stagOne)"
                    <> "AND (stagsALL ISNULL OR stagsALL <@ Tags)"
                    <> "AND (stagsIN ISNULL OR Tags && stagsIN)"
                    <> "AND (sheader ISNULL OR strpos(n.Header,sheader) > 0)"
                    <> "AND (scontent ISNULL OR strpos(n.Content,scontent) > 0)"
                    -- group elements to correct work array_agg(tag)
                    <> "GROUP BY n.Id, n.Header, n.RegDate, u.UserName, c.CatName, "
                    <> "n.Content, n.MainPhoto, n.Photos "
                    -- Add sorting
                    <> addSortBy nsort
                    -- Add pagination
                    <> getLimitOffsetBS param
                    <> ";")
                (nid,ndate,ndateLT,ndateGT,nauthor,ntag,ntagAll,ntagIn,nheader,ncont)
            return $ A.toJSON (news :: [News])
        _ -> throwError NotFound

addSortBy :: BS.ByteString -> BS.ByteString
addSortBy val = case val of
    "author"   -> " ORDER BY u.UserName "
    "date"     -> " ORDER BY n.RegDate "
    "category" -> " ORDER BY c.CatName "
    "photos"   -> " ORDER BY array_length (n.Photos,1) "
    _          -> ""
