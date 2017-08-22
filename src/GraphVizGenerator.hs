module GraphVizGenerator (genGraphViz) where

import Data.Char (toLower)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP

import Dependencies (Dependencies(..))
import ParserUtils (tableName)

genGraphViz :: String -> String
genGraphViz = intercalate "\n" . fmap genGraphVizFromDep . buildDeps

buildDeps :: String -> [Dependencies]
buildDeps s = case (readP_to_S dependencies . fmap toLower) s of
  [(deps, "")] -> deps
  otherwise    -> error "Failed to parse."

genGraphVizFromDep :: Dependencies -> String
genGraphVizFromDep (Dependencies tName fTables) =
  (intercalate "\n" . fmap (\fTable -> "  " ++ fTable ++ " -> " ++ tName)) fTables

dependencies :: ReadP [Dependencies]
dependencies = do
  skipSpaces
  deps <- sepBy dependency (string "\n\n")
  skipSpaces
  eof
  return deps

dependency :: ReadP Dependencies
dependency = do
  (tName:fTables) <- sepBy1 tableName (string "\n  ")
  return (Dependencies tName fTables)

