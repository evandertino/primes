{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib.Primes

main :: IO ()
main = do
  putStrLn "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  putStrLn (displayResult result)
