{-# LANGUAGE OverloadedStrings #-}
module Main where

import MarkDown.ToHTML
import MarkDown.Parser
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment (getArgs)
import qualified Data.Text.IO as T
import MyLib (someFunc)

main :: IO ()
main = someFunc
