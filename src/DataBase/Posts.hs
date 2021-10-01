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
import           Data.Time                          (Day)

import           DataBase
import           DataBase.Postgres
import           DataBase.Types

import           Exceptions
import           Logger
import           Prelude                            hiding (id)

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

newsToJSON :: Posts -> NewsJSON
newsToJSON Posts {..} = NewsJSON
    news_id
    news_header
    news_reg_date
    news_author
    news_category
    (fromPGArray news_tags)
    news_content
    news_main_photo
    (fromPGArray news_photos)

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
    news <- selectNewsWithParameters nid ndate ndateLT ndateGT nauthor
        ntag ntagAll ntagIn nheader ncont nsearch
        (addSortBy nsort) (getLimitOffsetBS param)
    return $ A.toJSON (newsToJSON <$> news)

addSortBy :: String -> BS.ByteString
addSortBy val = case val of
    "author"   -> " ORDER BY u.UserName "
    "date"     -> " ORDER BY n.RegDate "
    "category" -> " ORDER BY c.CatName "
    "photos"   -> " ORDER BY array_length (n.Photos,1) "
    _          -> ""
