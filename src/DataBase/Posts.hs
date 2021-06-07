{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module DataBase.Posts where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

import           GHC.Generics (Generic)


import           Control.Monad.Reader
import           Control.Monad.Except
import qualified Data.Aeson as A
import qualified Data.ByteString.UTF8 as BS
import           Data.Text ( Text )
import           Data.Text.Encoding (decodeUtf8,encodeUtf8)
import           Data.Maybe (fromMaybe)
import           Data.Time

import           DataBase
import           DataBase.Users
import           Exceptions
import           Logger

data News = News
    { nid :: Int
    , header :: Text
    , newsDate :: Day
    , authorName :: Text
    , catName :: Text
    , tags :: PGArray Text
    , content :: Text
    , mainPhoto :: Text
    , photos :: PGArray Text
    } deriving (Show,Eq)
instance A.ToJSON News where
    toJSON news = A.object 
        [ "id"          A..= nid news
        , "header"      A..= header news
        , "reg_date"    A..= newsDate news
        , "author"   A..= authorName news
        , "category" A..= catName news
        , "tags"     A..= fromPGArray (tags news)
        , "content"     A..= content news
        , "main_photo"  A..= mainPhoto news
        , "photos"      A..= fromPGArray (photos news)
        ]    
instance FromRow News where
    fromRow = News
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field

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
            let nid = fromMaybe "" $ getParam "id" param
            let ndate = fromMaybe "" $ getParam "created_at" param
            let ndateLT = fromMaybe "" $ getParam "created_at__lt" param
            let ndateGT = fromMaybe "" $ getParam "created_at__gt" param
            let nauthor = fromMaybe "" $ getParam "author" param
            let ntag = fromMaybe "" $ getParam "tag" param
            let ntagAll = fromMaybe "" $ getParam "tags__all" param
            let ntagIn = fromMaybe "" $ getParam "tags__in" param
            let nheader = fromMaybe "" $ getParam "header" param
            let ncont = fromMaybe "" $ getParam "content" param
            news <- liftIO $ queryDB pool $ Query $
                "SELECT n.Id, n.Header, n.RegDate, u.UserName, c.CatName, array_agg(t.tag), "
                <> "n.Content, n.MainPhoto, n.Photos  FROM News n "
                -- Add Name of Author
                <> " LEFT OUTER JOIN (Authors a JOIN Users u on a.userid = u.id)"
                <> " ON n.Author = a.id "  
                -- Add name of Category
                <> " LEFT OUTER JOIN Categories c ON n.Category = c.id "
                -- Add names of Tags
                <> "LEFT JOIN Tags t ON t.id = ANY(n.tags) "
                <> " WHERE n.Id > 0 " 
                -- Add selection
                <> addFieldToQueryNumBS "n.Id" nid 
                <> addFieldToQueryBS "n.RegDate" ndate 
                <> addFieldToQueryLaterBS "n.RegDate" ndateLT
                <> addFieldToQueryGraterBS "n.RegDate" ndateGT
                <> addAuthorByName nauthor
                <> addTag ntag
                <> addTagsAny ntagIn 
                <> addTagsAll ntagAll
                <> addFindTextToSelectBS "n.Header" nheader 
                <> addFindTextToSelectBS "n.Content" ncont
                -- group elements to correct work array_agg(tag)
                <> "GROUP BY n.Id, n.Header, n.RegDate, u.UserName, c.CatName, "
                <> "n.Content, n.MainPhoto, n.Photos "
                -- Add sorting
                <> addSortBy nsort
                -- Add pagination
                <> getLimitOffsetBS param
                <> ";"
            return $ A.toJSON (news :: [News])
        _ -> throwError NotFound

addAuthorByName :: BS.ByteString -> BS.ByteString
addAuthorByName val = if val == ""
    then ""
    else " AND strpos(u.UserName,'" <> val <> "')>0 " 

addTag :: BS.ByteString -> BS.ByteString
addTag tag = if tag == ""
    then ""
    else " AND Tags && ARRAY [" <> tag <> "]" 

addTagsAny :: BS.ByteString -> BS.ByteString
addTagsAny tags = if tags == ""
    then ""
    else " AND Tags && ARRAY" <> tags 

addTagsAll :: BS.ByteString -> BS.ByteString
addTagsAll tags = if tags == ""
    then ""
    else " AND ARRAY" <> tags <> " <@ Tags "

addSortBy :: BS.ByteString -> BS.ByteString
addSortBy val = case val of
    "author" -> " ORDER BY u.UserName "
    "date" -> " ORDER BY n.RegDate "
    "category" -> " ORDER BY c.CatName "
    "photos" -> " ORDER BY array_length (n.Photos,1) "
    _ -> ""