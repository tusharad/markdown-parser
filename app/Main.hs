{-# LANGUAGE OverloadedStrings #-}
module Main where

import MarkDown.ToHTML
import MarkDown.Parser
import MarkDown.Helper
import Text.Megaparsec
import Text.Megaparsec.Char
import MarkDown.HTTP.Core
import System.Environment (getArgs)
import qualified Data.Text.IO as T

main :: IO ()
main = runScotty 
  -- arg <- getArgs
  -- if length arg < 2 then error "Insufficent number of arguments"
  -- else do
  --   let inputFile = head arg
  --   let outputFile = arg !! 1
  --   fileContent <- T.readFile inputFile
  --   let res = parse (atom `sepBy` newline) "" fileContent
  --   case res of
  --     Left err -> print err
  --     Right parsedStructure -> do
  --       let x = joinMWord (joinMLine (concatParagraphs (concatTexts parsedStructure []) []) []) []
  --       let y = concatParagraphs x []
  --       T.writeFile outputFile (toHTML y)
