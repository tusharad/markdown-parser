{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           MarkDown.ToHTML
import           MarkDown.Common.Types
import           MarkDown.Parser
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
  describe "Testing ToHTML" $ do
    it "Heading" $ toHTML [MHeading (MWord "Hello") 3] `shouldBe` "<h3>Hello</h3> "
  
    it "Heading MLine" $ toHTML [MHeading (MLine [MWord "Hello",MWord "Nice",MWord "to",MWord "meet"]) 3] `shouldBe` "<h3>Hello Nice to meet </h3> "
    
    it "Italic" $ toHTML [MHeading (MLine [MWord "Hello",MItalic "Nice",MWord "to",MWord "meet"]) 3] `shouldBe` "<h3>Hello <italic>Nice</italic> to meet </h3> "
    
    it "ItalicBold" $ toHTML [MHeading (MLine [MWord "Hello",MItalicBold "Nice",MWord "to",MWord "meet"]) 3] `shouldBe` "<h3>Hello <italic><strong>Nice</strong></italic> to meet </h3> "
    
    it "LineQuotes" $ toHTML [MLineQuotes (MLine [MWord "Hello",MItalic "Nice",MWord "to",MWord "meet"])] `shouldBe` "<span>Hello <italic>Nice</italic> to meet  </span> "
    
    it "horizontal" $ toHTML [MHorizontal] `shouldBe` "<hr> "
    
    it "Ordered List" $ toHTML [MOrderedList (MLine [MWord "Hello",MWord "There"])] `shouldBe` "<li>Hello There  </li> "
    
    it "UnOrdered List" $ toHTML [MUnOrderedList (MLine [MWord "Hello",MWord "There"])] `shouldBe` "<li>Hello There  </li> "
    
    it "Code" $ toHTML [MCode "asd"] `shouldBe` "<code>asd</code> "
  describe "Testing Parser Functions" $ do
    it "Heading" $ parse parseHeading "" "# Hello there" `shouldBe` (Right $ (MHeading (MLine [MWord "Hello", MWord "there"]) 1))
