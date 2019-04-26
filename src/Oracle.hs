{-# LANGUAGE MonoLocalBinds #-}
{-
Title: Oracle utilities
Author: Pablo Baeyens
-}

module Oracle(
  Oracle
  ,buildOracle
  ,decodeOracle
  ,shape
  ,circuit
  )
where

import Quipper
import QuipperLib.ClassicalOptim
import qualified Data.Set as Set
import Text.Printf
import Control.Monad
import Data.Foldable

-- *** Basic types ***

-- | An oracle for a reversible function
data Oracle qa = Oracle {
  shape   :: qa, -- ^ The shape of the input
  circuit :: (qa,Qubit) -> Circ (qa,Qubit) -- ^ The circuit oracle
  }

-- | The type of truth table of an oracle
type CSV = [(String,String)]
type TruthTable = [([BoolParam], BoolParam)]
type Error = String

-- | Build an oracle from a non-reversible circuit
buildOracle
  :: QData qa
  => qa -- ^ Shape of input
  -> (qa -> Circ Qubit) -- ^ the circuit
  -> Oracle qa -- ^ the resulting oracle
buildOracle qa circ = Oracle qa (classical_to_reversible_optim circ)


--- *** Boolean parameters ***

-- | Read a list of parameters
readBoolParam :: String -> Either Error [BoolParam]
readBoolParam = mapM readParam
 where
  readParam :: Char -> Either Error BoolParam
  readParam '0' = Right PFalse
  readParam '1' = Right PTrue
  readParam x   = Left $ printf "illegal character: '%c'" x


-- *** Oracle reading ***

-- | Build Circuit from TruthTable using `build_circuit` directive
build_circuit
booleanFromTruth :: TruthTable -> [Bool] -> Bool
booleanFromTruth []          z = False
booleanFromTruth ((x, y):xs) z = if toBool x == z
  then newBool y
  else booleanFromTruth xs z
 where
  toBool []     = []
  toBool (x:xs) = newBool x : toBool xs


-- | Build a circuit from a truth table
-- Actual implementation on Bools is done on `booleanFromTruth`
-- Behaviour is undefined when truth table is ill-formed
fromTruthTable :: TruthTable -> [Qubit] -> Circ Qubit
fromTruthTable = unpack template_booleanFromTruth

-- | Decode unquoted csv-file
decode :: String -> Either Error CSV
decode = mapM decodeRow . filter (not . null) . lines
 where
  decodeRow row = do
    let (src, dst) = span (/= ',') row
    when (length dst < 2) (Left $ printf "malformed row: '%s'" row)
    pure (src, tail dst)


-- | Read truth table from string table.
-- Assumes non-empty table
toTruthTable :: CSV -> Either Error TruthTable
toTruthTable rawTable = do
  -- Create truth table and set of inputs
  (inputs, table) <- foldlM buildLine mempty rawTable

  -- Check set of inputs is complete
  when (length inputs /= 2 ^ n) (Left "predicate is partial")
  pure table
 where
  n = length $ fst $ head rawTable
  buildLine (inputs, truthTable) (x, y)
    | null x = Left "empty input"
    | x `elem` inputs = Left $ printf "malformed input: '%s' is duplicated" x
    | length x /= n = Left
    $ printf "malformed input: '%s' has size %d." x (length x)
    | length y /= 1 = Left
    $ printf "malformed output: '%s' is not single digit." y
    | otherwise = do
      x' <- readBoolParam x
      y' <- readBoolParam y
      pure (x `Set.insert` inputs, (x', head y') : truthTable)

-- | Get shape from truth table
-- Assumes non-empty table
getShape :: TruthTable -> Either Error [Qubit]
getShape xs = Right (replicate n qubit) where n = length $ fst $ head xs


-- | Read an oracle from a csv encoded string
decodeOracle :: String -> Either Error (Oracle [Qubit])
decodeOracle csv = do
  -- Decode raw table with elements as strings
  rawTable <- decode csv
  when (null rawTable) (Left "empty truth table")

  -- get truth table and shape
  table <- toTruthTable rawTable
  shape <- getShape table

  -- Build oracle
  let circ = fromTruthTable table
  pure (buildOracle shape circ)
