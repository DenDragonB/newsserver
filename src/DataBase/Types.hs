{-# LANGUAGE DeriveGeneric #-}

module DataBase.Types where

import qualified Data.Aeson                         as A
import           Data.Text                          (Text)
import           Data.Time                          (Day)
import           Database.PostgreSQL.Simple.FromRow (FromRow)
import           Database.PostgreSQL.Simple.Types   (PGArray)
import           GHC.Generics                       (Generic)

data Author = Author
    { author_id :: Int
    , user_id   :: Int
    , about     :: Text
    } deriving (Show,Eq,Generic)
instance A.ToJSON Author
instance FromRow Author

data Category = Category
    { category_id :: Int
    , name        :: Text
    , parent_id   :: Int
    } deriving (Show,Eq,Generic)
instance A.ToJSON Category
instance FromRow Category

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
