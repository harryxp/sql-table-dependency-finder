module ParserUtils (tableName) where

import Data.String.Utils (replace)

import Text.ParserCombinators.ReadP

tableName :: ReadP String
tableName = do
  optional (string "${prefix}")
  tName <- munch1 isTableNameChar
  (return . replace "." "__") tName

isTableNameChar :: Char -> Bool
isTableNameChar c =
  c >= 'a' && c <= 'z' ||
  c >= 'A' && c <= 'Z' ||
  c >= '0' && c <= '9' ||
  c == '_' ||
  c == '.'
