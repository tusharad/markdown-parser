{-# LANGUAGE DeriveAnyClass #-}
module MarkDown.Common.Types where

import Data.Text
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void Text

data MListItem = MListItem MarkDownElement [MarkDownElement]
  deriving (Eq,Show)

data MarkDownElement =
    MHeading        Int MarkDownElement
  | MParagraph      MarkDownElement
  | MBold           MarkDownElement
  | MItalic         MarkDownElement
  | MBoldItalic         MarkDownElement
  | MImage          MarkDownElement FilePath
  | MLink           MarkDownElement Text
  | MLine           [MarkDownElement]
  | MUnorderedList  [MListItem]
  | MOrderedList  [MListItem]
  | Only            Text                 -- Bottom of all types
  deriving (Eq,Show)
