-- | Module which contains the Bank Account function
module BankAccount(
                    bankAccountThread
                  ) where

import Datatypes ( Customer )
import RandomPick ( randomAmount, randomCustomer )
import Transactions ( transferTransaction )
import System.Random ( Random(randomRIO) )
import Control.Concurrent ( MVar, threadDelay, putMVar, takeMVar )

-- | Deals with the transactions of each customer
bankAccountThread :: MVar Int -> Customer -> [Customer] -> MVar String -- ^ Takes 4 inputs: an MVar Int, a Customer, a list of Customers, and an MVar String
                     -> IO () -- ^ Performs an IO action
bankAccountThread numberOfTotalTransactions threadOwner customers finished = do
    -- Extract number of transactions from the MVar, no other threads can access it and will wait here until it is free
    numOfTransactions <- takeMVar numberOfTotalTransactions 
    if numOfTransactions <= 100 then do -- Check if 100 or fewer transactions have been completed
        print $ "Count: " ++ show numOfTransactions -- Print the transaction number
        transferCustomer <- randomCustomer customers -- Get a random customer
        if threadOwner == transferCustomer then do -- Check if the random customer generated is the same as the current owner of the thread
            putMVar numberOfTotalTransactions numOfTransactions -- Put the number of transactions back in the MVar, allowing other threads to use it
            putStrLn "Same customer generated, randomising new customer..."
            bankAccountThread numberOfTotalTransactions threadOwner customers finished -- Recursively call this function
        else do
            amount <- randomAmount threadOwner -- Generated a random number between £10 and £50
            if amount > 0 then do -- Check if amount generated is more than 0 (it is set to 0 in the case of the balance being <10)
                transferTransaction threadOwner transferCustomer amount -- Transfer from thread owner to randomised customer
                putMVar numberOfTotalTransactions (numOfTransactions + 1) -- Add one to the number of transactions and put it back in the MVar
                delayTime <- randomRIO (10, 100) -- Generate random number to delay for
                threadDelay delayTime -- Delay in microseconds
                bankAccountThread numberOfTotalTransactions threadOwner customers finished -- Recursively call this function
            else do
                putMVar numberOfTotalTransactions numOfTransactions -- Close the MVar
                delayTime <- randomRIO (10, 100) -- Generate random number to delay for
                threadDelay delayTime -- Delay in microseconds
                bankAccountThread numberOfTotalTransactions threadOwner customers finished -- Recursively call this function
    else do
        putMVar numberOfTotalTransactions numOfTransactions -- Close the MVar
        putMVar finished "Completed" -- Make the 'finshed' MVar non-empty
        temp <- takeMVar finished -- Take the value of 'finished' out so other threads can finish
        print "Done!"
