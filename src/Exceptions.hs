module Exceptions where

import           Network.HTTP.Types

data Errors 
-- for WEB
    = UnknownRequest
    | NotFound
-- for Queries
    | WrongQueryParameter
    | ObjectExists    -- The object already exists
    | ObjectNOTExists -- The object is not exists on db    
-- for Users

-- for Authors
    | UserNOTExists

-- for Categories
    | CategoryWithSub
    | ParentNOTExists
-- for Tags

-- for Drafts
    | AuthorNOTExists

-- for Migrations
instance Show Errors where
    show UnknownRequest = "Unknown Request"
    show NotFound = "Not found"

    show WrongQueryParameter = "Wrong query parameter"

    show ObjectExists = "The object already exists"
    show ObjectNOTExists = " The object is not exists on db"

    show UserNOTExists = "The User is not exists on db"

    show AuthorNOTExists = "The Author is not exists on db"

    show CategoryWithSub = "Category have subcategories"
    show ParentNOTExists = "Category parent NOT exists"


errorCode :: Errors -> Status
errorCode err = case err of
    NotFound        -> status404
    _               -> status403