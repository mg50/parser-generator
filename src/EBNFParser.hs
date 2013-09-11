module EBNFParser where
import Text.ParserCombinators.Parsec
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M

data Rule = Rule String Pattern deriving (Eq, Show)
data Pattern = Token String
             | Sequence [Pattern]
             | InvokeRule String
             | Many Pattern
             | Many1 Pattern
             | Option Pattern
             | Alternation [Pattern]
             | Code String
             deriving (Eq, Show)

ebnfToken :: Parser Pattern
ebnfToken = do char '\''
               tok <- many $ noneOf "'*"
               char '\''
               return (Token tok)

invokeRule = do name <- ruleName
                return $ InvokeRule name

subPattern = modifier $ paren <|> ebnfToken <|> invokeRule
  where paren = do char '('
                   spaces
                   pat <- normalPattern
                   char ')'
                   spaces
                   return pat

modifier :: Parser Pattern -> Parser Pattern
modifier p = do pat <- p
                maybeMod <- optionMaybe (oneOf "+*?")
                return $ case maybeMod of
                  Just '*' -> Many pat
                  Just '+' -> Many1 pat
                  Just '?' -> Option pat
                  Nothing  -> pat

subPatternOrSeparator :: Parser (Maybe Pattern)
subPatternOrSeparator = sep <|> sub
  where sep   = do char '|'
                   return Nothing
        sub  = do pat <- modifier subPattern
                  return $ Just pat

groups :: [Maybe a] -> [[a]]
groups [] = []
groups (Nothing:xs) = groups xs
groups maybeAs = let (justAs, maybeAs') = span isJust maybeAs
                     as = map fromJust justAs
                 in if null maybeAs'
                    then [as]
                    else as : groups maybeAs'

maybeSequence :: [Pattern] -> Pattern
maybeSequence [] = undefined
maybeSequence (x:[]) = x
maybeSequence xs = Sequence xs

maybeAlternation :: [Pattern] -> Pattern
maybeAlternation [] = undefined
maybeAlternation (x:[]) = x
maybeAlternation xs = Alternation xs

codePattern :: Parser Pattern
codePattern = do char '`'
                 code <- many1 $ noneOf "`"
                 char '`'
                 return (Code code)

normalPattern :: Parser Pattern
normalPattern = do pat <- do pat <- subPattern
                             spaces
                             return pat
                   pats <- many $ do pat <- subPatternOrSeparator
                                     spaces
                                     return pat
                   let alts = map maybeSequence $ groups (Just pat:pats)
                   return $ if null pats
                               then pat
                               else maybeAlternation alts

ruleName :: Parser String
ruleName = do c <- letter
              cs <- many $ alphaNum <|> char '-'
              return (c:cs)

rule :: Parser Rule
rule = do name <- ruleName
          spaces
          char '='
          spaces
          pat <- codePattern <|> normalPattern
          return $ Rule name pat

parseRule :: String -> Rule
parseRule r = case parse rule "" r of
  Left err   -> error ("EBNF rule parse error: " ++ show err)
  Right rule -> rule

parseGrammar :: String -> [Rule]
parseGrammar gram = case parse parser "" gram of
  Left err   -> error ("EBNF grammar parse error: " ++ show err)
  Right rules -> rules
  where parser = many1 $ do r <- rule
                            eof <|> (many1 newline >> return ())
                            return r
