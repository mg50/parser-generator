module EBNFParserSpec where
import EBNFParser
import Test.Hspec

spec = do
  describe "groups" $ do
    let g :: [Maybe Int] -> [[Int]]
        g = groups

    it "handles an empty list" $ do
      g [] `shouldBe` []

    it "handles a list of one element" $ do
      g [Just 1] `shouldBe` [[1]]

    it "handles a list of severel elements" $ do
      g [Just 1, Just 2, Just 3] `shouldBe` [[1, 2, 3]]

    it "handles a list with Nothings inside" $ do
      g [Just 1, Just 2, Nothing, Just 3, Just 4] `shouldBe`
        [[1, 2], [3, 4]]

    it "handles a longer list" $ do
      g [Just 1, Just 2, Nothing, Just 3, Just 4, Nothing, Just 5] `shouldBe`
        [[1, 2], [3, 4], [5]]


  describe "rules" $ do
    it "parses a rule with one token" $ do
      parseRule "S = 'a'" `shouldBe` Rule "S" (Token "a")

    it "parses a rule with a weird name" $ do
      parseRule "asd-22fdaSdf = 'a'" `shouldBe` Rule "asd-22fdaSdf" (Token "a")

    it "parses a rule with a two token sequence" $ do
      parseRule "S = 'a' 'b'" `shouldBe` Rule "S" (Sequence [Token "a", Token "b"])

    it "parses a rule with another rule" $ do
      parseRule "S = 'a' S" `shouldBe` Rule "S" (Sequence [Token "a", InvokeRule "S"])

    it "parses a rule with Kleene star" $ do
      parseRule "S = 'a'*" `shouldBe` Rule "S" (Many (Token "a"))

    it "parses a rule with Kleene plus" $ do
      parseRule "S = 'a'+" `shouldBe` Rule "S" (Many1 (Token "a"))

    it "parses a rule with Kleene option" $ do
      parseRule "S = 'a'?" `shouldBe` Rule "S" (Option (Token "a"))

    it "parses a rule with several of these things" $ do
      parseRule "S = 'a'* T+ 'b'?" `shouldBe`
        Rule "S" (Sequence [Many (Token "a"),
                            Many1 (InvokeRule "T"),
                            Option (Token "b")])

    it "parses a rule with simple alternation" $ do
      parseRule "S = 'a' | 'b'" `shouldBe`
        Rule "S" (Alternation [Token "a", Token "b"])

    it "parses a rule with more complex alternation" $ do
      parseRule "S = 'a' U | S* 'blah' | T" `shouldBe`
        Rule "S" (Alternation [Sequence [Token "a",
                                         InvokeRule "U"],
                               Sequence [Many (InvokeRule "S"),
                                         Token "blah"],
                               InvokeRule "T"])

    it "parses a parenthesized pattern" $ do
      parseRule "S = ('hello')" `shouldBe`
        Rule "S" (Token "hello")

    it "parses a parenthesized pattern with spaces" $ do
      parseRule "S = ( 'hello' )" `shouldBe`
        Rule "S" (Token "hello")

    it "parses a parenthesized pattern with alternation and repetition" $ do
      parseRule "S = ( 'a' | 'b' )+ 'c' | T" `shouldBe`
        Rule "S" (Alternation [
                     Sequence [Many1 (Alternation [Token "a", Token "b"]),
                               Token "c"],
                     InvokeRule "T"
                  ])


    it "parses a code rule" $ do
      parseRule "S = `char 'a'`" `shouldBe` Rule "S" (Code "char 'a'")

  describe "grammar" $ do
    it "parses a simple grammar" $ do
      let gram = unlines ["r1 = `newline`",
                          "r2 = 'a'"]
      parseGrammar gram `shouldBe` [
        Rule "r1" (Code "newline"),
        Rule "r2" (Token "a")]

    it "parses a more complex grammar" $ do
      let gram = unlines ["r1 = `newline`",
                          "R2 = ( 'A' | 'B' )+ 'C' | R1"]
      parseGrammar gram `shouldBe` [
        Rule "r1" (Code "newline"),
        Rule "r2" (Alternation [
                     Sequence [Many1 (Alternation [Token "a", Token "b"]),
                               Token "c"],
                     InvokeRule "r1"
                  ])]
