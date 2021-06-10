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
    | WrongPass
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

instance Show Errors where
    show UnknownRequest = "Unknown Request"
    show NotFound = "404 - Not found"

    show WrongQueryParameter = "Wrong query parameter"

    show ObjectExists = "The object already exists"
    show ObjectNOTExists = " The object is not exists on db"

    show WrongPass = "Incorrect password or user is not exists"
    
    show UserNOTExists = "The User is not exists on db"

    show AuthorNOTExists = "The Author is not exists on db"

    show CategoryWithSub = "Category have subcategories"
    show ParentNOTExists = "Category parent NOT exists"

    show (Send fileName) = "Sending file: " <> fileName


errorCode :: Errors -> Status
errorCode err = case err of
    Send fileName   -> status200
    NotFound        -> status404
    _               -> status403