{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (intercalate,sort)
import Data.String.Utils (strip)
import GHC.Unicode (isSpace)
import Text.ParserCombinators.ReadP

main :: IO ()
main = getContents >>= print . parse

data Dependencies = Dependencies String [String]

instance Eq Dependencies where
  (Dependencies t1 d1) == (Dependencies t2 d2) = t1 == t2 && d1 == d2 -- assumption: d1 and d2 are already sorted

-- TODO there are better ways to chain comparisons
instance Ord Dependencies where
  compare (Dependencies t1 d1) (Dependencies t2 d2) =
    if t1 /= t2
    then compare t1 t2
    else let l1 = length d1
             l2 = length d2
         in if l1 /= l2
            then compare l1 l2
            else compare d1 d2  -- assumption: d1 and d2 are already sorted

instance Show Dependencies where
  show (Dependencies tbl deps) = tbl ++ "\n" ++ intercalate "\n" (("  "++) <$> deps)

-- TODO maximumBy is partial: error if deps is empty
parse :: String -> Dependencies
parse s = let deps = readP_to_S (singleBlock >>= \d -> eof >> return d) (strip s)
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

tableName :: ReadP String
tableName = do
  p <- option "" (string "${prefix}")
  n <- munch1 isTableNameChar
  return (p ++ n)

isTableNameChar :: Char -> Bool
isTableNameChar c =
  c >= 'a' && c <= 'z' ||
  c >= 'A' && c <= 'Z' ||
  c >= '0' && c <= '9' ||
  c == '_' ||
  c == '.'

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
  return (tName,sort fTables)

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
  optional (string "left" <++ string "right" <++ string "inner" <++ string "outer")
  skipSpaces1
  string "join"
  satisfy isSpace
