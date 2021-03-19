-- | Main module
module Main where

import BankAccount ( bankAccountThread )
import Datatypes ( Customer(Customer) )
import PrintInfo ( printFinalBalance )
import Control.Concurrent ( forkIO, newEmptyMVar, newMVar, takeMVar )
import Control.Concurrent.ParallelIO.Global ( parallel, stopGlobalPool )
import System.Environment ( getArgs )
-- | Main function, the start and end point of the program
main :: IO () -- ^ Performs an IO action
main = do
    -- Declare 10 MVars for each customers bank balance
    balance1 <- newMVar 1000
    balance2 <- newMVar 1000
    balance3 <- newMVar 1000
    balance4 <- newMVar 1000
    balance5 <- newMVar 1000
    balance6 <- newMVar 1000
    balance7 <- newMVar 1000
    balance8 <- newMVar 1000
    balance9 <- newMVar 1000
    balance10 <- newMVar 1000

    -- Declare all customers
    let cust1 = Customer "A" "1" balance1
    let cust2 = Customer "B" "2" balance2
    let cust3 = Customer "C" "3" balance3
    let cust4 = Customer "D" "4" balance4
    let cust5 = Customer "E" "5" balance5
    let cust6 = Customer "F" "6" balance6
    let cust7 = Customer "G" "7" balance7
    let cust8 = Customer "H" "8" balance8
    let cust9 = Customer "I" "9" balance9
    let cust10 = Customer "J" "10" balance10

    -- Declare list of customers
    let customers = [cust1, cust2, cust3, cust4, cust5, cust6, cust7, cust8, cust9, cust10]

    -- Initialise number of transactions to 1
    numberOfTotalTransactions <- newMVar 1

    -- Initialise finished to an empty MVar
    -- Used to make main wait until all transactions 
    -- have been completed before continuing
    finished <- newEmptyMVar

    -- Gets argument of program run call
    args <- getArgs

    -- Case statement to deal with argument of program call
    case args of
        ["concurrent"] -> do -- If called with 'concurrent' each account will perform IO actions  
                             -- independently in a new thread via the use of 'forkIO'
            forkIO $ bankAccountThread numberOfTotalTransactions cust1 customers finished
            forkIO $ bankAccountThread numberOfTotalTransactions cust2 customers finished
            forkIO $ bankAccountThread numberOfTotalTransactions cust3 customers finished
            forkIO $ bankAccountThread numberOfTotalTransactions cust4 customers finished
            forkIO $ bankAccountThread numberOfTotalTransactions cust5 customers finished
            forkIO $ bankAccountThread numberOfTotalTransactions cust6 customers finished
            forkIO $ bankAccountThread numberOfTotalTransactions cust7 customers finished
            forkIO $ bankAccountThread numberOfTotalTransactions cust8 customers finished
            forkIO $ bankAccountThread numberOfTotalTransactions cust9 customers finished
            forkIO $ bankAccountThread numberOfTotalTransactions cust10 customers finished

            w1 <- takeMVar finished -- Program will wait here until 'finished' is not empty
            putStrLn w1

            printFinalBalance customers -- Print the final balance of all customers
            putStrLn "Finished performing simulation using method: concurrent"

        ["parallel"] -> do -- If called with 'concurrent' each account will perform IO actions  
                           -- independently in a new thread via the use of 'forkIO'
            let threads = [bankAccountThread numberOfTotalTransactions cust1 customers finished,
                           bankAccountThread numberOfTotalTransactions cust2 customers finished,
                           bankAccountThread numberOfTotalTransactions cust3 customers finished,
                           bankAccountThread numberOfTotalTransactions cust4 customers finished,
                           bankAccountThread numberOfTotalTransactions cust5 customers finished,
                           bankAccountThread numberOfTotalTransactions cust6 customers finished,
                           bankAccountThread numberOfTotalTransactions cust7 customers finished,
                           bankAccountThread numberOfTotalTransactions cust8 customers finished,
                           bankAccountThread numberOfTotalTransactions cust9 customers finished,
                           bankAccountThread numberOfTotalTransactions cust10 customers finished]
            parallel threads
            putStrLn "Reading accounts"
            printFinalBalance customers -- Print the final balance of all customers
            stopGlobalPool
            putStrLn "Finished performing simulation using method: parallel"

        _ -> do
            putStrLn "Incorrect argument, please use either: 'concurrent' or 'parallel'"