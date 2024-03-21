{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  QuasiQuotes #-}
module MarkDown.ToHTML where

import           MarkDown.Common.Types
import qualified Data.Text as T

eval :: MarkDown -> T.Text
eval md = case md of
  MHeading txt n     -> T.pack $ "<h" <> show n <> ">" <> T.unpack (eval txt) <> "</h" <> show n <> ">"
  MLine mdList       -> toHTML mdList
  MBold txt          -> "<strong>" <> txt <> "</strong>"
  MItalic txt        -> "<italic>" <> txt <> "</italic>"
  MItalicBold txt    -> "<italic><strong>" <> txt <> "</strong></italic>"
  MLineQuotes md'    -> "<span>" <> helper md' "" <> "</span>"
  MHorizontal        -> "<hr>"
  MOrderedList md'   -> "<li>" <> helper md' "" <> "</li>"
  MUnOrderedList md' -> "<li>" <> helper md' "" <> "</li>"
  MCode txt          -> "<code>" <> txt <> "</code>"
  MEscapeCode txt    -> "<span>" <> txt <> "</span>"
  MLink txt link     -> "<a href=\"" <> link <> "\">" <> txt <> "</a>"
  MImage txt link    -> "<a href=\"" <> link <> "\">" <> txt <> "</a>"
  MParagraph txt     -> "<p>" <> txt <> "</p>"
  MWord txt          -> txt
  _                  -> "Something went wrong"

helper :: MarkDown -> T.Text -> T.Text
helper md txt = eval md <> " " <> txt

toHTML :: [MarkDown] -> T.Text
toHTML = foldr helper ""
