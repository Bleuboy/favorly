module Favorly where

import Control.Concurrent.STM
import System.IO.Unsafe

-- Define a data type for User with a name and credits
data User = User { 
    name :: String,    -- The user's name
    credits :: Int     -- The number of credits the user has
} deriving (Show)

-- Define a list of example users
allUsersRef :: IORef [User]
allUsersRef = unsafePerformIO $ newIORef [User "Alice" 5, User "Bob" 5, User "Charlie" 5]

allUsers :: [User]
allUsers = unsafePerformIO $ readIORef allUsersRef

findUser :: String -> [User] -> User
findUser name users = head $ filter (\u -> name == name u) users

-- Function to perform a favor to multiple users
performFavor :: User -> [User] -> Int -> Either String ([User], User)
performFavor giver receivers numCredits
    | numCredits <= 0 = Left "invalid favor value. favor value must be positive."
    | null receivers = Left "no recipient specified for the favor."
    | otherwise =
        let 
            -- calculate the fraction of credits each receiver should pay
            fraction = numCredits `div` length receivers

            -- update the giver's credits by adding the received credits
            newGiver = giver { credits = (credits giver) + numCredits }

            -- update the receivers' credits by subtracting the fraction of credits
            updatedReceivers = map (\receiver -> receiver { credits = (credits receiver) - fraction }) receivers
        in 
            Right (updatedReceivers, newGiver)

-- Function to transfer credits from one user to multiple users
transferCredits :: User -> [User] -> Int -> Either String ([User], User)
transferCredits giver receivers numCredits
    | numCredits <= 0 = Left "invalid favor value. favor value must be positive."
    | credits giver >= numCredits = performFavor giver receivers numCredits
    | otherwise = Left "insufficient credits to perform the favor"

-- function to check if a favor is possible given the available credits
isValidFavor :: User -> Int -> Bool
isValidFavor user numCredits = credits user >= numCredits

-- function to display user information
displayUserInfo :: User -> IO ()
displayUserInfo user = do
    putStrLn ("user: " ++ (name user))
    putStrLn ("credits: " ++ (show $ credits user))

-- main function to demonstrate the code
main :: IO ()
main = do
    let user1 = User { name = "Alice", credits = 10 }
    let user2 = User { name = "Bob", credits = 10 }
    let user3 = User { name = "Charlie", credits = 10 }

    putStrLn "before favor:"
    displayUserInfo user1
    displayUserInfo user2
    displayUserInfo user3

    putStrLn "\nperforming favor to multiple users:"
    case transferCredits user1 [user2, user3] 6 of
        Left errorMsg -> putStrLn $ "error: " ++ errorMsg
        Right (newUsers, newUser1) -> do
            putStrLn "\nafter favor:"
            mapM_ displayUserInfo newUsers
            displayUserInfo newUser1

    putStrLn "\nattempting favor with negative credits:"
    case transferCredits user1 [user2, user3] (-2) of
        Left errorMsg -> putStrLn $ "error: " ++ errorMsg
        Right _ -> putStrLn "favor successful (unexpected)."

    putStrLn "\nattempting favor with zero credits:"
    case transferCredits user1 [user2, user3] 0 of
        Left errorMsg -> putStrLn $ "error: " ++ errorMsg
        Right _ -> putStrLn "favor successful (unexpected)."

    putStrLn "\nattempting favor without specifying recipient:"
    case performFavor user1 [] 6 of
        Left errorMsg -> putStrLn $ "error: " ++ errorMsg
        Right _ -> putStrLn "favor successful (unexpected)."
