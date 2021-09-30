{-# LANGUAGE OverloadedStrings #-}

module TestWeb where

import qualified Data.Aeson                   as A
import qualified Data.Aeson.Encoding.Internal as A
import           Data.ByteString.Builder      (lazyByteString)
import qualified Data.ByteString.UTF8         as BS
import           Network.HTTP.Types
import           Network.Wai.Internal
import           Test.Hspec

import           Web

param :: [( BS.ByteString , Maybe BS.ByteString )]
param = [ ("id",Just "1")
        , ("name",Just "name")
        ]

main :: IO ()
main = hspec $ do
    describe "Web functions" $ do
        it "paramToText" $ do
            let res = paramToText param
            res `shouldBe`
                [ ("id",Just "1")
                , ("name",Just "name")
                ]

