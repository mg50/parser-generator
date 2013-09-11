module GenParser where
import EBNFParser

variables = xyz ++ [ l ++ show i | i <- [1..], l <- xyz]
  where xyz = ["x", "y", "z"]

-- ruleToHs :: Rule -> String
-- ruleToHs (Rule name pat) = name ++ " = do " ++ patternToHs offset pat
--   where offset = length name ++ 6

patternToHs (Token str) = "string " ++ show str
