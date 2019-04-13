module Lib.PrimesSpec where

import           Lib.Primes                     ( primes
                                                , sieve
                                                , isPrime
                                                , primeFactors
                                                )
import Data.Either
import           Data.Maybe


prop_validPrimesOnly val = if val < 2 || val >= length primes
  then isLeft result
  else isRight result
  where result = isPrime val

prop_primesArePrime val = result /= Right True || null divisors
 where
  result   = isPrime val
  divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite val = result /= Right False || not (null divisors)
 where
  result   = isPrime val
  divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal val =
  isNothing result || product (fromJust result) == val
  where result = primeFactors val

prop_allFactorsPrime val = isNothing result || all (== Right True) resultsPrime
 where
  result       = primeFactors val
  resultsPrime = map isPrime (fromJust result)
