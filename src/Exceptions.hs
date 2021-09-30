module Exceptions where

import           Control.Exception
import           Data.Typeable
import           Network.HTTP.Types

data Errors
-- for WEB
    = UnknownRequest
    | NotFound
-- for Queries
    | PostgreError
    | WrongQueryParameter String
    | ParametrParseError String
    | ObjectExists    -- The object already exists
    | ObjectNOTExists -- The object is not exists on db
-- for Users
    | WrongPass
    | NoToken
-- for Authors
    | UserNOTExists

-- for Categories
    | CategoryWithSub
    | ParentNOTExists
-- for Tags

-- for Drafts
    | AuthorNOTExists

-- for snding files
    | Send String
    deriving (Typeable,Eq)
instance Exception Errors
instance Show Errors where
    show UnknownRequest           = "Unknown Request"
    show NotFound                 = "404 - Not found"

    show PostgreError             = "Error when accessing the database. Contact your administrator."
    show (WrongQueryParameter par)= "Wrong query parameter " <> par
    show (ParametrParseError par) = "Cannot parse parameter " <> par
    show ObjectExists             = "The object already exists"
    show ObjectNOTExists          = "The object is not exists on db"

    show WrongPass                = "Incorrect password or user is not exists"
    show NoToken                  = "Token exists in requiest"

    show UserNOTExists            = "The User is not exists on db"

    show AuthorNOTExists          = "The Author is not exists on db"

    show CategoryWithSub          = "Category have subcategories"
    show ParentNOTExists          = "Category parent NOT exists"

    show (Send fileName)          = "Sending file: " <> fileName


errorCode :: Errors -> Status
errorCode err = case err of
    Send _   -> status200
    NotFound -> status404
    _        -> status403
