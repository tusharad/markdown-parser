{-# LANGUAGE OverloadedStrings #-}
module MarkDown.Parser where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal.Search as T
import           Data.Void
import           Control.Monad (void)
import           Data.Maybe
import          MarkDown.Common.Types


parseHeading :: Parser MarkDownElement
parseHeading = do
  space
  hashes <- takeWhile1P Nothing (=='#')
  let cnt = T.length hashes
  MHeading cnt <$> parseLine

parseListItem :: Parser a -> Parser MListItem
parseListItem numberingParser = do
  space
  void numberingParser
  content <- takeWhile1P Nothing (/='\n')
  subItems <- optional $ try (some (newline >> char ' ' >> char ' ' >> parseLine))
  void $ optional $ newline
  case parseMaybe parseLine content of
    Nothing -> return $ MListItem (Only content) $ fromMaybe [] subItems
    Just parsedContent -> return $ MListItem parsedContent $ fromMaybe [] subItems

parseOrderedList :: Parser MarkDownElement
parseOrderedList = MOrderedList <$> some (parseListItem (some digitChar >> char '.'))

parseUnorderedList :: Parser MarkDownElement
parseUnorderedList = MUnorderedList <$> some (parseListItem (char '-'))

-- To create a line break or new line (<br>), end a line with two or more 
-- spaces, and then type return.
parseParagraph :: Parser MarkDownElement
parseParagraph = do
  space
  content <- takeWhile1P Nothing (/='\n')
  return $ MParagraph (Only content)

parseBoldItalic :: Parser MarkDownElement
parseBoldItalic = space >> parseBold_  "***" <|> parseBold_ "___"
  where
    parseBold_ str = do
      space
      void $ string str
      content <- lookAhead $ takeWhile1P Nothing (/='\n')
      let res = T.indices str content
      case res of
        []    -> parseError (TrivialError 1 Nothing mempty)  -- if the ending ** is not found, parseBold will fail
        (indexOfStarStar:_) -> do
          void $ takeP Nothing indexOfStarStar
          void $ string str
          let contentWithoutStarStar = T.take indexOfStarStar content
          case parseMaybe parseLineForBold contentWithoutStarStar of
            Nothing -> return $ MBoldItalic (Only contentWithoutStarStar)
            Just x -> return $ MBoldItalic x

parseBold :: Parser MarkDownElement
parseBold = space >> parseBold_  "**" <|> parseBold_ "__"
  where
    parseBold_ str = do
      space
      void $ string str
      content <- lookAhead $ takeWhile1P Nothing (/='\n')
      let res = T.indices str content
      case res of
        []    -> parseError (TrivialError 1 Nothing mempty)  -- if the ending ** is not found, parseBold will fail
        (indexOfStarStar:_) -> do
          void $ takeP Nothing indexOfStarStar
          void $ string str
          let contentWithoutStarStar = T.take indexOfStarStar content
          case parseMaybe parseLineForBold contentWithoutStarStar of
            Nothing -> return $ MBold (Only contentWithoutStarStar)
            Just x -> return $ MBold x

parseItalic :: Parser MarkDownElement
parseItalic = space >> go '*' <|> go '_'
  where
    go ch = do
      space
      void $ char ch
      content <- lookAhead $ takeWhile1P Nothing (/='\n')
      let mRes = T.findIndex (==ch) content
      case mRes of
        Nothing    -> parseError (TrivialError 1 Nothing mempty)  -- if the ending ** is not found, parseBold will fail
        Just indexOfStarStar -> do
          void $ takeP Nothing indexOfStarStar
          void $ char ch
          let contentWithoutStarStar = T.take indexOfStarStar content
          case parseMaybe parseLineForBold contentWithoutStarStar of
            Nothing -> return $ MItalic (Only contentWithoutStarStar)
            Just x -> return $ MItalic x


parseImage :: Parser MarkDownElement
parseImage = do
  space
  void $ char '!'
  imageText <- between (char '[') (char ']') parseLineForImage
  imageSrc <- between (char '(') (char ')') (takeWhileP Nothing (/=')'))
  return $ MImage imageText (T.unpack imageSrc)

-- Make sure to put parseLink behind parseImage
-- Since every link will be parsed by image
parseLink :: Parser MarkDownElement
parseLink = do
  space
  linkText <- between (char '[') (char ']') parseLineForLink
  linkURL <- between (char '(') (char ')') (takeWhileP Nothing (/=')'))
  space
  return $ MLink linkText linkURL

excludeCharacters :: [Char]
excludeCharacters = [
    '#'
  , '!'
  , '\n'
  , '_'
  , '*'
  , '['
  , ']'
  ]

parseOnly :: Parser MarkDownElement
parseOnly = do
  space
  content <- takeWhile1P Nothing (`notElem` excludeCharacters)
  return $ Only content

parseLine :: Parser MarkDownElement
parseLine = MLine <$> some (choice parserList)

parseLineForImage :: Parser MarkDownElement
parseLineForImage = MLine <$> some (choice parserListForImage)

parseLineForLink :: Parser MarkDownElement
parseLineForLink = MLine <$> some (choice parserListForLink)


parseLineForItalic :: Parser MarkDownElement
parseLineForItalic = MLine <$> some (choice parserListForItalic)

parseLineForBold :: Parser MarkDownElement
parseLineForBold = MLine <$> some (choice parserListForBold)

parseEverythingTillStarOrUnderscore :: Parser MarkDownElement
parseEverythingTillStarOrUnderscore = do
  content <- takeWhile1P Nothing (\x -> (x/='*') && (x/='\n') && (x/='_'))
  return $ Only content

parserList :: [Parser MarkDownElement]
parserList = [
        try parseBoldItalic
      , try parseBold
      , try parseItalic
      , try parseUnorderedList
      , try parseOrderedList
      , try parseImage
      , try parseLink
      , try parseOnly
      ]

parserListForItalic :: [Parser MarkDownElement]
parserListForItalic = [
        try parseBold
      , try parseImage
      , try parseUnorderedList
      , try parseOrderedList
      , try parseLink
      , try parseOnly
      , try parseEverythingTillStarOrUnderscore
      ]

parserListForBold :: [Parser MarkDownElement]
parserListForBold = [
        try parseItalic
      , try parseImage
      , try parseUnorderedList
      , try parseOrderedList
      , try parseLink
      , try parseOnly
      , try parseEverythingTillStarOrUnderscore
      ]

parserListForImage :: [Parser MarkDownElement]
parserListForImage = [
       try parseBoldItalic
      , try parseBold
      , try parseItalic
      , try parseUnorderedList
      , try parseOrderedList
      , try parseLink
      , try parseOnly
      ]

parserListForLink :: [Parser MarkDownElement]
parserListForLink = [
        try parseBoldItalic
      , try parseBold
      , try parseItalic
      , try parseUnorderedList
      , try parseOrderedList
      , try parseImage
      , try parseOnly
      ]


mainParser :: Parser MarkDownElement
mainParser = choice [
                try parseHeading
              , try parseLine
              , try parseBoldItalic
              , try parseBold
              , try parseItalic
              , try parseUnorderedList
              , try parseOrderedList
              , try parseImage
              , try parseLink
              , try parseParagraph
              ]

test :: IO ()
test = putStrLn "Hello World!"
