{-# LANGUAGE DeriveAnyClass #-}
module MarkDown.Common.Types where

import Data.Text
import Data.Void (Void)
import Text.Megaparsec
import GHC.Generics
import Data.Aeson

type Parser = Parsec Void Text

data MarkDown =
      MLine          [MarkDown]
    | MOL            [MarkDown]
    | MLineQuotes    MarkDown
    | MOrderedList   MarkDown
    | MUnOrderedList MarkDown
    | MHeading       MarkDown Int 
    | MBold          Text
    | MItalic        Text
    | MItalicBold    Text
    | MCode          Text
    | MEscapeCode    Text
    | MLink          Text Text
    | MImage         Text Text
    | MWord          Text
    | MParagraph     Text
    | MBlockQuotes   Text
    | MHorizontal
    deriving (Show,Eq)

data InputMarkDown = InputMarkDown { message :: Text } deriving (Show,Generic,FromJSON)
