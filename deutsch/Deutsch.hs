{-# LANGUAGE MonoLocalBinds #-}
{-
Title: Deutsch-Jozsa algorithm
Author: Pablo Baeyens
-}

module Deutsch(
  deustchJozsa
  ,getType
  ) where

import Oracle (Oracle, shape, circuit)
import Quipper
import QuipperLib.Simulation

-- | The type of an oracle
data OracleType =
  Balanced     -- ^ A balanced oracle
  | Constant   -- ^ A constant oracle
  deriving (Eq, Show)


-- | Implements the Deustch algorithm.
--  Given an oracle computes the circuit of Deutsch algorithm
-- (see generalized version at `deustchJozsa`)
deustch :: ((Qubit,Qubit) -> Circ (Qubit,Qubit)) -> Circ Bit
deustch oracle = do
  -- Initialize x (to False) and y (to True)
  (x,y) <- qinit (False,True)

  -- Apply Hadamard gates to each qubit
  (x,y) <- map_hadamard (x,y)

  -- Apply oracle
  (x,y) <- oracle (x,y)

  -- Apply Hadamard gate again to x
  x <- map_hadamard x

  -- Measure and discard
  z <- measure x
  qdiscard y

  pure z


-- | Implements the Deustch-Jozsa algorithm.
--  Given an oracle computes the circuit of Deutsch-Jozsa algorithm
deustchJozsa :: (QShape ba qa ca) => Oracle qa -> Circ ca
deustchJozsa oracle = do
  -- Initialize x (to False) and y (to True)
  (x, y) <- qinit $ (qc_false (shape oracle), True)

  -- Apply Hadamard gates to each qubit
  (x, y) <- map_hadamard (x, y)

  -- Apply oracle
  (x, y) <- boxedOracle (x, y)

  -- Apply Hadamard gate again to x
  x      <- map_hadamard x

  -- Measure and discard
  z      <- measure x
  qdiscard y
  pure z
  where boxedOracle = box "Oracle" $ circuit oracle


-- | Get type of an oracle using Deutsch-Jozsa algorithm
getType :: Oracle [Qubit] -> IO OracleType
getType oracle = do
  -- The Deutsch-Josza circuit for the given oracle
  let deustchCircuit = deustchJozsa oracle

  -- Simulate Deustch-Jozsa circuit
  result <- run_generic_io (0 :: Double) deustchCircuit

  -- Print list of measured results
  print result

  -- if any of the qubits on the result is True, the oracle is balanced,
  --  otherwise it is constant.
  pure (if or result then Balanced else Constant)