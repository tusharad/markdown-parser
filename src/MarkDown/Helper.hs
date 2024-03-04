{-# LANGUAGE OverloadedStrings #-}
module MarkDown.Helper where

import MarkDown.Common.Types

concatParagraphs :: [MarkDown] -> [MarkDown] -> [MarkDown]
concatParagraphs [] res = reverse res
concatParagraphs [x] res = reverse $ x : res
concatParagraphs ((MParagraph x):(MParagraph y):xs) res = concatParagraphs (MParagraph (x <> "%%" <> y):xs) res
concatParagraphs (x:xs) res = concatParagraphs xs (x:res)

concatTexts :: [MarkDown] -> [MarkDown] -> [MarkDown]
concatTexts [] res               = reverse res
concatTexts ((MLine lst):xs) res = concatTexts xs ((MLine $ joinMWord lst []):res)
concatTexts (x:xs) res = concatTexts xs (x:res)

joinMLine :: [MarkDown] -> [MarkDown] -> [MarkDown]        
joinMLine [] res  = res
joinMLine [x] res  = reverse $ x:res
joinMLine ((MLine x):(MLine y):xs) res = joinMLine ((MLine (x <> y)):xs) res
joinMLine (x:xs) res = joinMLine xs (x:res)

joinMWord :: [MarkDown] -> [MarkDown] -> [MarkDown]        
joinMWord [] res  = res
joinMWord [x] res  = reverse $ x:res
joinMWord ((MWord x):(MWord y):xs) res = joinMWord ((MWord (x <> " " <> y)):xs) res
joinMWord (x:xs) res = joinMWord xs (x:res)
