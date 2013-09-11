module EBNFParser where
import Test.Hspec

spec = do
  it "parses a simple grammar" $ do
    parseGrammar "S = 'a'" `shouldBe` Rule "S" (Token "a")
