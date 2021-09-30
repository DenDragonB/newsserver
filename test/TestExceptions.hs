module TestExceptions where

import           Network.HTTP.Types
import           Test.Hspec

import           Exceptions

main :: IO ()
main = hspec $ do
    describe "Exceptions functions" $ do
        it "errorCode 404" $ do
            let res = errorCode NotFound
            res `shouldBe` status404
        it "errorCode 200" $ do
            let res = errorCode (Send "abc")
            res `shouldBe` status200
        it "errorCode 403" $ do
            let res = errorCode WrongPass
            res `shouldBe` status403
