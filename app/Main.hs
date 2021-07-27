module Main where

import Control.Monad
import Data.List (nub)
import Prover
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Flag
  = PropSize String
  | Samples String
  | Help
  deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option
      "n"
      ["size"]
      (ReqArg PropSize "n")
      "Size of random propositions.",
    Option
      "s"
      ["samples"]
      (ReqArg Samples "s")
      "Numbers of random samples.",
    Option
      "h?"
      ["help"]
      (NoArg Help)
      "Print this help message"
  ]

usageHeader :: String
usageHeader = "Usage: prop-prover [OPTION...]"

usage :: IO a
usage = do
  putStr $ usageInfo usageHeader options
  exitSuccess

parse :: [String] -> IO [Flag]
parse argv = case getOpt Permute options argv of
  (opts, _, [])
    | Help `elem` opts -> usage
    | otherwise -> return opts
  (_, _, errs) -> usage

getSize :: [Flag] -> IO Int
getSize (PropSize s : _) = return $ read s
getSize (_ : xs) = getSize xs
getSize _ = usage

getSamples :: [Flag] -> IO Int
getSamples (Samples s : _) = return $ read s
getSamples (_ : xs) = getSamples xs
getSamples _ = usage

main :: IO ()
main = do
  ops <- getArgs >>= parse
  size <- getSize ops
  samples <- getSamples ops
  replicateM_ samples (proveRandom size)
  exitSuccess
