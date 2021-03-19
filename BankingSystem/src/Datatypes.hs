-- | Module which contains the Customer datatype and associated print functions
module Datatypes(
                 Customer(Customer, name, accountNumber, balance)
                ) where

import Control.Concurrent ( MVar )

-- | Customer datatype definition
data Customer = Customer {
    name :: String, -- ^ Stores name as a String
    accountNumber :: String, -- ^ Stores account number as a String
    balance :: MVar Rational -- ^ Stores balance as an MVar Rational
} deriving (Eq)
