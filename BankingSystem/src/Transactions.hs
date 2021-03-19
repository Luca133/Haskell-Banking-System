-- | Module which contains functions to perform transactions
module Transactions(
                    transferTransaction
                   ) where

import Datatypes(Customer(Customer, name, balance, accountNumber))
import Control.Concurrent ( putMVar, takeMVar )

-- | Perform a credit transaction on a Customer's account
creditTransaction :: Customer -> Rational -- ^ Takes two inputs: a Customer and a Rational
                     -> IO () -- ^ Performs an IO action
creditTransaction previousCustomer amountToAdd = do
    tempbalance <- takeMVar (balance previousCustomer)
    let newBalance = tempbalance + amountToAdd
    putMVar (balance previousCustomer) newBalance

-- | Performs a debit transaction on a Customer's account
debitTransaction :: Customer -> Rational -- ^ Takes two inputs: a Customer and a Rational
                    -> IO () -- ^ Performs an IO action
debitTransaction previousCustomer amountToRemove = do
    tempbalance <- takeMVar (balance previousCustomer)
    let newBalance = tempbalance - amountToRemove
    putMVar (balance previousCustomer) newBalance

-- | Perform a transaction between two accounts
transferTransaction :: Customer -> Customer -> Rational -- ^ Takes three inputs: two Customers and a Rational
                       -> IO () -- ^ Performs an IO action
transferTransaction payFrom payTo amount = do
    debitTransaction payFrom amount
    creditTransaction payTo amount
    putStrLn "Transfer complete"