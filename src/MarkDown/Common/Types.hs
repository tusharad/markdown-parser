module MarkDown.Common.Types where

import Data.Text
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void Text

data MarkDown =  
      MHeading Text Int 
    | MBold Text
    | MItalic Text
    | MItalicBold Text
    | MLineQuotes MarkDown
    | MBlockQuotes Text
    | MHorizontal
    | MOrderedList Text
    | MUnOrderedList Text
    | MCode Text
    | MEscapeCode Text
    | MLink Text Text
    | MImage Text Text
    | MText Text
    | MParagraph Text
    | MLine [MarkDown]
    deriving (Show,Eq)

