{-# LANGUAGE OverloadedStrings #-}
module MarkDown.HTTP.Core where
import           Web.Scotty
import           MarkDown.Common.Types
import           MarkDown.Parser
import qualified Data.Text.Lazy as TI

runScotty :: IO ()
runScotty = scotty 3000 $ do
      post "/toHTML" $ do
        inputText <- (jsonData :: ActionM InputMarkDown)
        res <- liftIO $ processMD inputText
        html $ TI.fromStrict $ res
