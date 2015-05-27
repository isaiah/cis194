module Ch02Spec (spec) where

import           Ch02
import           Log
import           Test.Hspec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "parses an individual line from the log file" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      parseMessage "This is not in right format" `shouldBe` Unknown "This is not in right format"
  describe "insert" $ do
    it "inserts log message to correct position" $ do
      let m1 = LogMessage Info 1 "xxx"
          m2 = LogMessage Info 2 "xxx"
          m3 = LogMessage Info 3 "xxx"
          m4 = LogMessage Info 4 "xxx"
          expected =
            Node (Node Leaf m1 Leaf) m2 (Node (Node Leaf m3 Leaf) m4 Leaf)

          tree = Node (Node Leaf m1 Leaf) m2 (Node Leaf m4 Leaf)
      insert m3 tree `shouldBe` expected
