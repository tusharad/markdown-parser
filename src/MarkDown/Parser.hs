{-# LANGUAGE OverloadedStrings #-}
module MarkDown.Parser (module MarkDown.Parser,module Text.Megaparsec) where

import Text.Megaparsec
import Text.Megaparsec.Char
import MarkDown.Common.Types
import qualified Data.Text as T
import MarkDown.Helper
import MarkDown.ToHTML

processMD :: InputMarkDown -> IO T.Text
processMD inputText = do
     let res = parse (atom `sepBy` newline) "" (message inputText)
     case res of
       Left err -> (print err) >> pure "Parsing failed!"
       Right parsedStructure -> do
         let x = joinMWord (joinMLine (concatParagraphs (concatTexts parsedStructure []) []) []) []
         let y = concatParagraphs x []
         pure (toHTML y)

atom :: Parser MarkDown
atom = do
    choice [
          parseHeading
        , parseHorizontalLine
        , parseLineQuotes
        , parseOrderedList
        , parseUnorderedList
        , parseLine
        ]

parseLine :: Parser MarkDown
parseLine = do
    res <- takeWhileP Nothing (/= '\n')
    let x = parse ((try parseItalicBold <|> try parseBold <|> try parseItalic <|>  try parseLink <|> try parseEscapeCode <|>  try parseCode <|> try parseText) `sepBy` (char ' ')) "" res
    case x of
      Left _ -> return (MLine [MParagraph res])
      Right r -> return (MLine r)

parseText :: Parser MarkDown
parseText = do
    res <- takeWhileP Nothing (/= ' ')
    return (MWord res)

parseHeading :: Parser MarkDown
parseHeading = do
    _ <- char '#'
    res <- takeWhileP (Just "number of #'s") (=='#')
    space
    headingText <- takeWhileP Nothing (/='\n')
    case parse atom "" headingText of
        Left _ -> pure (MHeading (MWord headingText) (T.length res + 1)) 
        Right r -> pure (MHeading r (T.length res + 1))

parseBold :: Parser MarkDown
parseBold = do
    res <- between (string "**") (string "**")  (takeWhileP Nothing (/= '*')) <|>
            between (string "__") (string "__")  (takeWhileP Nothing (/= '_'))
    return (MBold res)

parseItalic :: Parser MarkDown
parseItalic = do
    res <- between (char '*') (char '*') (takeWhileP Nothing (/= '*')) <|>
            between (char '_') (char '_') (takeWhileP Nothing (/= '_'))
    pure (MItalic res)

parseItalicBold :: Parser MarkDown
parseItalicBold = do
    res <- between (string "***") (string "***") (takeWhileP Nothing (/= '*')) <|>
            between (string "___") (string "___") (takeWhileP Nothing (/= '_'))
    pure (MItalicBold res)

parseLineQuotes :: Parser MarkDown
parseLineQuotes = do
    _ <- char '>'
    _ <- space
    res <- takeWhileP Nothing (/= '\n')
    case parse atom "" res of
        Left _ -> pure (MLineQuotes $ MParagraph res)
        Right r -> pure (MLineQuotes r)

parseOrderedList :: Parser MarkDown
parseOrderedList = do
    _ <- some digitChar
    _ <- char '.'
    space
    res <- takeWhileP Nothing (/='\n')
    case parse parseLine "" res of
        Left _ -> pure (MOrderedList (MWord res))
        Right r -> pure (MOrderedList r)

parseUnorderedList :: Parser MarkDown
parseUnorderedList = do
    _ <- char '*' <|> char '-' <|> char '+'
    space
    res <- takeWhileP Nothing (/='\n')
    case parse parseLine "" res of
        Left _ -> pure (MUnOrderedList (MWord res))
        Right r -> pure (MUnOrderedList r)

parseLink :: Parser MarkDown
parseLink = do
    _ <- char '!'
    alt <- between (char '[') (char ']') (takeWhileP Nothing (/=']'))
    path <-  between (char '(') (char ')') (takeWhileP Nothing (/=')'))
    pure (MLink alt path)

parseCode :: Parser MarkDown
parseCode = do
    res <- between (char '`') (char '`') (takeWhileP Nothing (/='`'))
    pure (MCode res)

parseEscapeCode :: Parser MarkDown
parseEscapeCode = do
    res <- between (string "``") (string "``") (takeWhileP Nothing (/='`'))
    pure (MEscapeCode ("`" <> res <> "`"))

parseHorizontalLine :: Parser MarkDown
parseHorizontalLine = do
    _ <- string "---" <|> string "***"
    _ <- char '\n' 
    pure MHorizontal
