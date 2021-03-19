-- | Module which contains functions to generate random values
module RandomPick(
                  randomCustomer,
                  randomAmount
                 ) where

import Datatypes(Customer(balance))
import System.Random ( Random(randomRIO) )
import Data.Ratio.Rounding(dpRound)
import Control.Concurrent ( readMVar )

-- | Generate a random rational value
randomRational :: IO Rational -- ^ Takes no input and performs an IO rational action
randomRational = do
    numberGeneratedBuiltDouble <- randomRIO (10 :: Double, 50 :: Double)
    let numberGeneratedBuiltRational = toRational numberGeneratedBuiltDouble
    return (dpRound 2 numberGeneratedBuiltRational)

-- | Generate a random amount to transfer
randomAmount :: Customer -- ^ Takes as input a Customer
                -> IO Rational -- ^ Performs an IO rational action
randomAmount customer = do
    bal <- readMVar (balance customer)
    if bal < 10 then return $ toRational 0
    else do
        randomValue <- randomRational
        if bal < randomValue then 
            randomAmount customer
        else return randomValue

-- | Generate a random customer
randomCustomer :: [Customer] -- ^ Takes as input a list of Customers
                  -> IO Customer -- ^ Performs an IO Customer action
randomCustomer customers = do
    x <- (randomRIO (0, 1) :: IO Double)
    if x < 0.1 then do
        putStrLn "Customer A"
        return $ head customers
    else if x < 0.2 then do
        putStrLn "Customer B"
        return $ customers !! 1
    else if x < 0.3 then do
        putStrLn "Customer C"
        return $ customers !! 2
    else if x < 0.4 then do
        putStrLn "Customer D"
        return $ customers !! 3
    else if x < 0.5 then do
        putStrLn "Customer E"
        return $ customers !! 4
    else if x < 0.6 then do
        putStrLn "Customer F"
        return $ customers !! 5
    else if x < 0.7 then do
        putStrLn "Customer G"
        return $ customers !! 6
    else if x < 0.8 then do
        putStrLn "Customer H"
        return $ customers !! 7
    else if x < 0.9 then do
        putStrLn "Customer I"
        return $ customers !! 8
    else do
        putStrLn "Customer J"
        return $ last customers