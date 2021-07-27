module Samplers
  ( randomBools,
    randomPart,
  )
where

import Control.Monad
import Control.Monad.Random
import Data.Array.ST
import GHC.Arr
import Math.Combinat.Partitions.Integer (fromPartition, randomPartition)

-- | Generates n random boolean values.
randomBools :: Int -> IO [Bool]
randomBools n = replicateM n randomIO

-- | Generates a random partition of n variables.
--   Note: It generates a random partition of n, say,
--   generates that many variables (with repetition),
--   and returns a shuffled variant of it.
randomPart :: Int -> IO [String]
randomPart n = do
  xs <- getStdRandom $ randomPartition n
  let xs' = toVariables (fromPartition xs)
  evalRandIO $ shuffle xs'

toVariables :: [Int] -> [String]
toVariables = concatMap (\k -> replicate k ("x" ++ show k))

shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
  let l = length xs
  rands <- forM [0 .. (l - 2)] $ \i -> getRandomR (i, l - 1)
  let ar = runSTArray $ do
        ar' <- thawSTArray $ listArray (0, l - 1) xs
        forM_ (zip [0 ..] rands) $ \(i, j) -> do
          vi <- readSTArray ar' i
          vj <- readSTArray ar' j
          writeSTArray ar' j vi
          writeSTArray ar' i vj
        return ar'
  return (elems ar)
