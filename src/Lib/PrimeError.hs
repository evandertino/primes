module Lib.PrimeError where

data PrimeError = TooLarge | InvalidValue deriving Eq

instance Show PrimeError where
  show TooLarge     = "Value exceeds limits of prime checker"
  show InvalidValue = "Numbers less than 2 are not candidates for primes"