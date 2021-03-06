{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DataBase.Types where

import qualified Data.Aeson                         as A
import           Data.Text                          (Text)
import           Data.Time                          (Day)
import           Database.PostgreSQL.Simple.FromRow (FromRow)
import           Database.PostgreSQL.Simple.Types   (PGArray)
import           GHC.Generics                       (Generic)

data Author = Author
    { id :: Int
    , user_id   :: Int
    , about     :: Text
    } deriving (Show,Eq,Generic)
instance A.ToJSON Author
instance FromRow Author

data Category = Category
    { id   :: Int
    , name :: Text
    , parent_id     :: Int
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

data Posts = Posts
    { news_id         :: Int
    , news_header     :: Text
    , news_reg_date   :: Day
    , news_author     :: Text
    , news_category   :: Text
    , news_tags       :: PGArray Text
    , news_content    :: Text
    , news_main_photo :: Text
    , news_photos     :: PGArray Text
    } deriving (Show,Eq,Generic)
instance FromRow Posts

data TagType = TagType
    { id   :: Int
    , name :: Text
    } deriving (Show,Eq,Generic)
instance A.ToJSON TagType
instance FromRow TagType

data User = User
    { id    :: Int
    , first_name :: String
    , last_name  :: String
    , avatar     :: String
    , username   :: String
    , upass      :: Maybe String
    , reg_date   :: Day
    , adm        :: Maybe Bool
    , token      :: Maybe String
    } deriving (Show,Eq,Generic)
instance A.ToJSON User
instance FromRow User

data UserToGet = UserToGet
    { id    :: Int
    , first_name :: String
    , last_name  :: String
    , avatar     :: String
    , username   :: String
    , reg_date   :: Day
    } deriving (Show,Eq,Generic)
instance A.ToJSON UserToGet
instance FromRow UserToGet
