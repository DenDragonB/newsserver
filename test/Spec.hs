module Main where

import qualified TestDataBase
import qualified TestExceptions
import qualified TestLogger
import qualified TestWeb


main :: IO ()
main = do
    TestLogger.main
    TestWeb.main
    TestDataBase.main
    TestExceptions.main
