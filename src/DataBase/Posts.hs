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
    , autorId :: Int
    , catId :: Int
    , tags :: PGArray Int
    , content :: Text
    , mainPhoto :: Text
    , photos :: PGArray Text
    } deriving (Show,Eq)
instance A.ToJSON News where
    toJSON news = A.object 
        [ "id"          A..= nid news
        , "header"      A..= header news
        , "reg_date"    A..= newsDate news
        , "author_id"   A..= autorId news
        , "category_id" A..= catId news
        , "tags_id"     A..= fromPGArray (tags news)
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
            let nid = fromMaybe "" $ getParam "id" param
            let ndate = fromMaybe "" $ getParam "created_at" param
            let author = fromMaybe "" $ getParam "author" param
            news <- liftIO $ queryDB pool $ Query $
                "SELECT * FROM News WHERE Id > 0 " 
                <> addToUpdateNum "Id" nid 
                <> addToUpdateNum "RegDate" ndate 
                <> addAuthorByName author
                <> ";"
            return $ A.toJSON (news :: [News])
        _ -> throwError NotFound

addAuthorByName :: BS.ByteString -> BS.ByteString
addAuthorByName val = if val == ""
    then ""
    else ", Author = (SELECT Id FROM Authors WHERE "
        <> "UserId = (SELECT Id FROM Users WHERE UserName = "
        <> val <> "))" 