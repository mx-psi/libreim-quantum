module Classical(
  toffoli
  ,nand
  ,random
  ,fanout
  ,notCirc
  ,andCirc
  ,wire
  ,xnor) where
import Quipper
import QuipperLib.ClassicalOptim

-- | Cable
wire :: Qubit -> Circ Qubit
wire x = do
  pure x

-- | Puerta Toffoli
toffoli :: (Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit)
toffoli (x, y, z) = do
  z <- qnot z `controlled` (x, y)
  pure (x, y, z)


-- | The (simulated) NAND gate
nand :: (Qubit, Qubit) -> Circ Qubit
nand (x, y) = do
  a         <- qinit True
  (x, y, a) <- toffoli (x, y, a)
  qdiscard (x, y)
  pure a


-- | The (simulated) RANDOM gate
random :: Circ Qubit
random = do
  x <- qinit False
  x <- hadamard x
  pure x


-- | The (simulated) FANOUT gate
fanout :: Qubit -> Circ (Qubit, Qubit)
fanout x = do
  y         <- qinit True
  z         <- qinit False

  (x, y, z) <- toffoli (x, y, z)

  qterm True y -- Terminamos con valor 1
  pure (x, z)


notCirc :: Qubit -> Circ Qubit
notCirc x = fanout x >>= nand

andCirc :: (Qubit, Qubit) -> Circ Qubit
andCirc (x, y) = nand (x, y) >>= notCirc


build_circuit
boolean_xnor (x, y) = (not x || y) && (x || not y)

xnor :: (Qubit, Qubit) -> Circ Qubit
xnor = unpack template_boolean_xnor
