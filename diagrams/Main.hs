module Main where

import Data.Foldable

import Quipper
import Quipper.Printing
import QuipperLib.Simulation

import System.Directory

import Classical
import Print

bellPair :: Circ (Qubit, Qubit)
bellPair = do
  (x, y, z) <- qinit (True, False, False)
  hadamard y
  toffoli (x, y, z)
  qterm True x
  pure (y, z)

img :: FilePath
img = "img/"

examples :: [(FilePath, IO ())]
examples = map
  (\(f, m) -> (img ++ f, m))
  [ ("nand.pdf"   , print_generic PDF nand (qubit, qubit))
  , ("toffoli.pdf", print_generic PDF toffoli (qubit, qubit, qubit))
  , ("random.pdf" , print_generic PDF random)
  , ("fanout.pdf" , print_generic PDF fanout qubit)
  , ("not.pdf"    , print_generic PDF notCirc qubit)
  , ("and.pdf"    , print_generic PDF andCirc (qubit, qubit))
  , ("wire.pdf"   , print_generic PDF wire qubit)
  , ("xnor.pdf"   , print_generic PDF xnor (qubit, qubit))
  , ("bell.pdf"   , print_generic PDF bellPair)
  ]

main :: IO ()
main = do
  createDirectoryIfMissing False         img
  for_                     examples      (uncurry captureTo)
  run_generic_io           (0 :: Double) bellPair >>= print
