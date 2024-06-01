{-# LANGUAGE OverloadedStrings #-}
module MarkDown.ToHTML where

import           MarkDown.Common.Types
import qualified Data.Text as T
import  Data.Text (Text)

toHTML :: [MarkDownElement] -> Text
toHTML = foldr (\x acc -> toHTMLHelper x <> acc) ""

toHTMLHelper :: MarkDownElement -> Text
toHTMLHelper (MHeading n md) = "<h" <> T.pack (show n) <> ">"
    <> toHTMLHelper md <> "</h" <> T.pack (show n) <> ">"
toHTMLHelper (MParagraph md) = "<p>" <> toHTMLHelper md <> "</p>"
toHTMLHelper (MBold md) = "<strong>" <> toHTMLHelper md <> "</strong>"
toHTMLHelper (MItalic md) = "<i>" <> toHTMLHelper md <> "</i>"
toHTMLHelper (MBoldItalic md) = "<strong><i>" <> toHTMLHelper md <> "</i></strong>"
toHTMLHelper (MImage md filePath) = "<img src=\"" <> T.pack filePath <> "\" />"
toHTMLHelper (MLink md link) = "<a href=\"" <> link <> "\">" <> toHTMLHelper md <> "</a>"
toHTMLHelper (MLine md) = toHTML md
toHTMLHelper (MUnorderedList itemList) = "<ol>" <> itemToHTML itemList <> "</ol>"
toHTMLHelper (MOrderedList itemList) = "<ul>" <> itemToHTML itemList <> "</ul>"
toHTMLHelper (Only txt) = txt

itemToHTML :: [MListItem] -> Text
itemToHTML = foldr (\x acc -> "<li>" <> itemToHTML_ x <> "</li>" <> acc) ""

itemToHTML_ :: MListItem -> Text
itemToHTML_ (MListItem md mds) = toHTMLHelper md <> toHTML mds