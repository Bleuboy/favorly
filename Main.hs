{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Heist
import qualified Text.XmlHtml as X
import qualified Data.Text as T
import Data.Monoid
import Control.Monad.Trans
import Data.IORef
import Favorly

-- Define routes
app :: SpockM () () () ()
app = do
    get root $ do
        -- Render the main page template
        (render "main")

    post "performFavor" $ do
        -- Get form data
        giverName <- param "giverName"
        receiverNames <- param "receiverNames"
        numCredits <- param "numCredits"

        -- Perform the favor and get the updated users
        liftIO $ atomically $ do
            let giver = findUser giverName allUsers
            let receivers = map (\name -> findUser name allUsers) (T.words receiverNames)
            case transferCredits giver receivers (read (T.unpack numCredits)) of
                Left errorMsg -> putStrLn errorMsg
                Right (newUsers, _) -> writeIORef usersRef newUsers

        -- Redirect to the main page after performing the favor
        redirect "/"

-- Define Heist config
config :: SpockCfg () () () ()
config = defaultSpockCfg () PCNoDatabase ()

-- Define main function to run the web server
main :: IO ()
main = do
    -- Load Heist templates
    heist <- initHeist $ emptyHeistConfig

    -- Run the Spock web server
    usersRef <- newIORef allUsers
    runSpock 8080 (spock config (app heist usersRef))
