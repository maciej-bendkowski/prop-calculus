module BinTree
  ( BinTree (..),
    randomBinTreeIO,
  )
where

import Control.Monad.Random
import Control.Monad.ST
import Data.Array.ST

-- | Data type for plane binary trees.
data BinTree
  = Node BinTree BinTree
  | Leaf
  deriving (Show)

algR :: Int -> StdGen -> (BinTree, StdGen)
algR 0 g = (Leaf, g)
algR n g = runST $ do
  arrL <- newArray (0, 2 * n) 0
  (arrL', g') <- algR' arrL g n 0
  r <- readArray arrL' 0
  (t, g'') <- buildTree arrL' r g'
  return (t, g'')

randomBinTreeIO :: Int -> IO BinTree
randomBinTreeIO n = getStdRandom (algR n)

algR' ::
  STUArray s Int Int ->
  StdGen ->
  Int ->
  Int ->
  ST s (STUArray s Int Int, StdGen)
algR' arrL g n i
  | n == i = return (arrL, g)
  | otherwise = do
    let (x, g') = randomR (0, 4 * i + 1) g

    let (k, b) = x `divMod` 2
    let i' = i + 1

    writeArray arrL (2 * i' - b) (2 * i')

    z <- readArray arrL k
    writeArray arrL (2 * i' - 1 + b) z

    writeArray arrL k (2 * i' - 1)
    algR' arrL g' n i'

buildTree ::
  STUArray s Int Int ->
  Int ->
  StdGen ->
  ST s (BinTree, StdGen)
buildTree arrL n g = do
  lc <- readArray arrL n
  (nodeL, g') <-
    if even lc
      then return (Leaf, g)
      else buildTree arrL lc g

  rc <- readArray arrL (n + 1)
  (nodeR, g'') <-
    if even rc
      then return (Leaf, g')
      else buildTree arrL rc g'

  return (Node nodeL nodeR, g'')
