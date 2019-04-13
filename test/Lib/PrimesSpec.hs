module Lib.PrimesSpec where

import           Data.Either
import           Data.Maybe
import           Lib.Primes                     ( primes
                                                , sieve
                                                , isPrime
                                                , primeFactors
                                                , displayResult
                                                )
import           Lib.PrimeError

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

prop_displayResultsWorksFineWithValue val =
  result_1 || result_2 || result_3 || result_4
 where
  primeVal = isPrime val
  display  = displayResult primeVal
  result_1 = primeVal == Right True && display == "It's prime"
  result_2 = primeVal == Right False && display == "It's composite"
  result_3 = primeVal == Left TooLarge && display == show TooLarge
  result_4 = primeVal == Left InvalidValue && display == show InvalidValue
