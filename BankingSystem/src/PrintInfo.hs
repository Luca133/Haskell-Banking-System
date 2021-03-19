-- | Module which deals with printing customers data
module PrintInfo(
                 printFinalBalance
                ) where

import Datatypes ( Customer(Customer) )
import Control.Concurrent ( MVar, readMVar )

-- | Print the details of one customer
printCustomerInfo :: Customer -- ^ Takes a Customer as an input
                     -> IO () -- ^ Performs an IO action
printCustomerInfo (Customer name accountNumber balance) = do
    currentBalance <- readMVar balance
    putStrLn $ "\n Customer name: " ++ name ++ "\n Account number: " ++ accountNumber ++ "\n Balance: " ++ show (fromRational currentBalance :: Double)

-- | Print balance of all customers
printFinalBalance :: [Customer] -- ^ Takes a list of Customers as an input
                     -> IO () -- ^ Performs an IO action
printFinalBalance [] = putStrLn ""
printFinalBalance (x:xs) = do
    printCustomerInfo x
    printFinalBalance xs