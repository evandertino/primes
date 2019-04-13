import           Test.QuickCheck
import           Lib.PrimesSpec

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Running tests..."
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime
  quickCheck prop_displayResultsWorksFineWithValue
  putStrLn "done!"
