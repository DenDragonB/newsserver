module DataBase.Types where

import           Database.PostgreSQL.Simple.FromRow
import qualified Data.Aeson                         as A
import           GHC.Generics

data Author = Author
    { id      :: Int
    , user_id :: Int
    , about   :: Text
    } deriving (Show,Eq,Generic)
instance A.ToJSON Author
instance FromRow Author