module Main where
{-
Title: Main module
Author: Pablo Baeyens
Usage: `./$0` reads from stdin and unquoted csv file that defines a truth table.
       `./$0 file` does it from the file
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.Directory

import Deutsch
import Oracle (decodeOracle)
import Quipper
import Print

import Data.Foldable

-- | Usage information
usage :: String
usage =
  "usage: `./deutsch input.csv` or `./deutsch`.\n\n"
    ++ "Read from input.csv (or stdin) the truth table \nof a balanced or constant predicate,\n"
    ++ "formatted as a csv file without quotes.\n"
    ++ "Example: f : Bool² → Bool function f(x) = 0 should be given as:\n"
    ++ "  00,0\n  01,0\n  10,0\n  11,0\n"
    ++ "Run Deutsch-Jozsa algorithm on it.\n"


-- | Reports an error an signals failure
reportErr :: String -> IO a
reportErr err = putStr ("error: " ++ err ++ "\n" ++ usage) >> exitFailure

-- | Get the correct handle.
-- Either returns a correct handle or signals failure
getHandle :: IO Handle
getHandle = do
  args <- getArgs
  case args of
    []     -> return stdin  -- No file given, falling back to stdin
    [file] -> fromFile file -- Get handle from file
    _      -> reportErr "only one positional argument (INPUT) allowed."
 where
    -- | Opens file if it exists, otherwise signals failure
  fromFile :: FilePath -> IO Handle
  fromFile file = do
    exists <- doesFileExist file
    if exists
      then openFile file ReadMode
      else reportErr $ "Requested file " ++ show file ++ " does not exist."

-- | Main function
main :: IO ()
main = do
  -- Read Truth table from appropiate handle
  handle <- getHandle
  contents <- hGetContents handle

  case decodeOracle contents of
    Left err -> reportErr err -- there has been an error
    Right oracle -> do
      -- Preview the circuit
      captureTo "deustch.pdf" $ print_generic PDF (deustchJozsa oracle)

      -- Run Deustch-Jozsa algorithm
      oracleType <- getType oracle
      putStrLn $ "Oracle type: " ++ show oracleType
