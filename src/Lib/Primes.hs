module Lib.Primes
  ( primes
  , sieve
  , isPrime
  , primeFactors
  )
where

primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve []                 = []
sieve (nextPrime : rest) = nextPrime : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Maybe Bool
isPrime n | n < 2              = Nothing
          | n >= length primes = Nothing
          | otherwise          = Just (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 []                    = []
unsafePrimeFactors _ []                    = []
unsafePrimeFactors n (next : primeNumbers) = if n `mod` next == 0
  then next : unsafePrimeFactors (n `div` next) (next : primeNumbers)
  else unsafePrimeFactors n primeNumbers

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2              = Nothing
               | n >= length primes = Nothing
               | otherwise          = Just (unsafePrimeFactors n primeLessThanN)
  where primeLessThanN = filter (<= n) primes
