module Lib.PrimesSpec where

import           Lib.Primes                     ( primes
                                                , sieve
                                                , isPrime
                                                , primeFactors
                                                )
import           Data.Maybe


prop_validPrimesOnly val = if val < 2 || val >= length primes
  then isNothing result
  else isJust result
  where result = isPrime val

prop_primesArePrime val = result /= Just True || null divisors
 where
  result   = isPrime val
  divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite val = result /= Just False || not (null divisors)
 where
  result   = isPrime val
  divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal val =
  isNothing result || product (fromJust result) == val
  where result = primeFactors val

prop_allFactorsPrime val = isNothing result || all (== Just True) resultsPrime
 where
  result       = primeFactors val
  resultsPrime = map isPrime (fromJust result)
