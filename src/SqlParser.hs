module SqlParser (parseSql) where

import Data.Char (toLower)
import Data.List (nub,sort)
import Data.String.Utils (strip)
import GHC.Unicode (isSpace)
import Text.ParserCombinators.ReadP

import Dependencies (Dependencies(..))
import ParserUtils (tableName)

-- TODO maximum is partial: error if deps is empty
parseSql :: String -> Dependencies
parseSql s = let deps = (readP_to_S (singleBlock >>= \d -> eof >> return d) . strip . fmap toLower) s
  in (fst . maximum) deps

singleBlock :: ReadP Dependencies
singleBlock = do
  tName1 <- dropStmt
  skipSpaces
  (tName2,fTables) <- createStmt
  if tName1 /= tName2
  then pfail
  else return (Dependencies tName1 fTables)
  return (Dependencies tName1 fTables)

dropStmt :: ReadP String
dropStmt = do
  string "drop"
  skipSpaces1
  string "table"
  skipSpaces1
  string "if"
  skipSpaces1
  string "exists"
  skipSpaces1
  tName <- tableName
  skipSpaces
  char ';'
  return tName

skipSpaces1 :: ReadP ()
skipSpaces1 = satisfy isSpace >> skipSpaces

createStmt :: ReadP (String,[String])
createStmt = do
  string "create"
  optional (skipSpaces1 >> string "temp")
  skipSpaces1
  string "table"
  skipSpaces1
  tName <- tableName
  skipSpaces1
  string "as"
  skipSpaces1
  string "select"
  satisfy isSpace
  skipMany1 get
  satisfy isSpace
  string "from"
  skipSpaces1
  fTables <- fromTables
  return (tName,(sort . nub) fTables)

fromTables :: ReadP [String]
fromTables = sepBy fromTable ((skipSpaces >> join       >> skipSpaces) <++
                              (skipSpaces >> (char ',') >> skipSpaces))

fromTable :: ReadP String
fromTable = do
  tName <- tableName
  -- optional (skipSpaces1 >> tableName) -- alias
  skipMany get
  return tName

join :: ReadP Char
join = do
  optional (string "left" <++ string "right" <++ string "cross" <++ string "inner" <++ string "outer")
  skipSpaces1
  string "join"
  satisfy isSpace
