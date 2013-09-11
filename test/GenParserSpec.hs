module GenParserSpec where
import GenParser
import Test.Hspec

spec = do
  describe "variables" $ do
    it "has the correct list of variables" $ do
      take 9 variables `shouldBe` ["x", "y", "z", "x1", "y1", "z1",
                                   "x2", "y2", "z2"]
