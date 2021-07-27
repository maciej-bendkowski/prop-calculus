module Prover
  ( proveRandom,
    intuitionistic,
    intuitionistic',
    classical,
    classical',
  )
where

import Proposition
import System.Exit
import System.Process
import System.Timeout

-- | Generates a random proposition and runs
--   two provers to check if it's an intuitionistic
--   or classical theorem.
proveRandom :: Int -> IO ()
proveRandom n = do
  x <- randomProp n
  runProver x >>= print

runProver :: Proposition -> IO (Maybe Bool, Maybe Bool)
runProver p = do
  let p' = transform p
  intI <- intuitionistic p'
  intC <- do
    case intI of
      Just False -> return (Just False)
      _ -> classical p'

  return (intI, intC)

timeoutValue :: Integer
timeoutValue = 180

-- | Checks if the given proposition is a classical
--   tautology. Note: `Nothing` indicates a solver timeout.
classical :: Proposition -> IO (Maybe Bool)
classical p = timeout (fromInteger $ timeoutValue * 1000000) (classical' p)

classical' :: Proposition -> IO Bool
classical' p = do
  (code, _, _) <-
    readProcessWithExitCode
      "int-solver"
      [ show $ transform $ doubleNegation p,
        show timeoutValue
      ]
      ""
  case code of
    ExitSuccess -> return True
    ExitFailure _ -> return False

-- | Checks if the given proposition is a classical
--   tautology. Note: `Nothing` indicates a solver timeout.
intuitionistic :: Proposition -> IO (Maybe Bool)
intuitionistic p = timeout (fromInteger $ timeoutValue * 1000000) (intuitionistic' p)

intuitionistic' :: Proposition -> IO Bool
intuitionistic' p = do
  (code, _, _) <- readProcessWithExitCode "int-solver" [show p, show timeoutValue] ""
  case code of
    ExitSuccess -> return True
    ExitFailure _ -> return False
