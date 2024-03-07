module MarkDown.Common.Types where

import Data.Text
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void Text

data MarkDown =
      MHeading       MarkDown Int 
    | MBold          Text
    | MItalic        Text
    | MItalicBold    Text
    | MLineQuotes    MarkDown
    | MBlockQuotes   Text
    | MOL            [MarkDown]
    | MOrderedList   MarkDown
    | MUnOrderedList MarkDown
    | MCode          Text
    | MEscapeCode    Text
    | MLink          Text Text
    | MImage         Text Text
    | MWord          Text
    | MParagraph     Text
    | MLine          [MarkDown]
    | MHorizontal
    deriving (Show,Eq)
