{-# LANGUAGE OverloadedStrings #-}
module MarkDown.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import MarkDown.Common.Types
import qualified Data.Text as T

atom :: Parser MarkDown
atom = do
    choice [
          parseHeading
        , parseLineQuotes
        , parseHorizontalLine
        , parseOrderedList
        , parseUnorderedList
        , parseEscapeCode
        , try parseLine
        , try parseItalicBold
        , try parseBold
        , try parseItalic
        , try parseLink
        , try parseCode
        , parseParagraph
        ]

parseLine :: Parser MarkDown
parseLine = do
    res <- takeWhileP Nothing (/= '\n')
    let x = parse ((parseItalicBold <|> parseBold <|> parseItalic <|> parseLink <|> parseEscapeCode <|>  parseCode <|> parseText) `sepBy` (char ' ')) "" res
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

parseParagraph :: Parser MarkDown
parseParagraph = do
    res <- takeWhileP Nothing (/= '\n')
    pure (MParagraph res)

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
    space
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
    pure MHorizontal