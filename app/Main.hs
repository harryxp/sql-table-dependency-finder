{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)

import SqlParser (parseSql)
import GraphVizGenerator (genGraphViz)

main :: IO ()
main = getArgs >>= \args -> case args of
  []        -> getContents >>= print . parseSql
  ["--gv"]  -> getContents >>= putStr . genGraphViz
  otherwise -> error "Illegal argument(s)."

