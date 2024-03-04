{-# LANGUAGE OverloadedStrings #-}
module MarkDown.ToHTML where
import MarkDown.Common.Types
import Data.Text as T

toHeading :: MarkDown -> String
toHeading (MHeading txt n) = "<h" <> show n <> ">" <> T.unpack (toHTML [txt] "") <> "</h" <> show n <> ">"
toHeading _ = error "Expected MHeading type"

toHTML :: [MarkDown] -> Text -> Text
toHTML [] res = res
toHTML ((MHeading x y):xs) res    = toHTML xs (res <> T.pack (toHeading (MHeading x y)))
toHTML ((MLine lst):xs) res       = toHTML xs (res <> toHTML lst "")
toHTML ((MWord ""):xs) res        = toHTML xs res
toHTML ((MWord txt):xs) res       = toHTML xs (res <> " " <> txt <> " ")
toHTML ((MParagraph ""):xs) res  = toHTML xs res
toHTML ((MParagraph " "):xs) res  = toHTML xs res
toHTML ((MParagraph txt):xs) res  = toHTML xs (res <> "<p>" <> txt <> "</p>")
toHTML ((MItalic txt):xs) res     = toHTML xs (res <> "<em>" <> txt <> "</em>")
toHTML ((MBold txt):xs) res       = toHTML xs (res <> "<strong>" <> txt <> "</strong>")
toHTML ((MItalicBold txt):xs) res = toHTML xs (res <> "<strong><em>" <> txt <> "</em></strong>")
toHTML ((MLineQuotes x):xs) res   = toHTML xs (res <> "<span>" <> toHTML [x] "" <> "</span>")
toHTML ((MBlockQuotes x):xs) res  = toHTML xs (res <> "<span>" <> x <> "</span>")
toHTML (MHorizontal:xs) res     = toHTML xs (res <> "<hr>")
toHTML ((MCode x):xs) res         = toHTML xs (res <> "<code>" <> x <> "</code>")
toHTML ((MLink txt link):xs) res  = toHTML xs (res <> "<a href=\"" <> link <> "\">" <> txt <> "</a>")
toHTML ((MOrderedList item1):(MOrderedList item2):xs) res  = toHTML (MOrderedList (MParagraph $ "<li>" <> toHTML [item1] "" <> "</li>" <> "<li>" <> toHTML [item2] "" <> "</li>"):xs) res
toHTML ((MUnOrderedList item1):(MUnOrderedList item2):xs) res  = toHTML (MUnOrderedList (MParagraph $ "<li>" <> toHTML [item1] "" <> "</li>" <> "<li>" <> toHTML [item2] "" <> "</li>"):xs) res
toHTML ((MOrderedList item):xs) res  = toHTML xs (res <> "<ol><li>" <> toHTML [item] "" <> "</li></ol>")
toHTML ((MUnOrderedList item):xs) res  = toHTML xs (res <> "<ul><li>" <> toHTML [item] "" <> "</li></ul>")
toHTML (_:xs) res  = toHTML xs (res <>"<p>Parsing failed</p>")
