module Main where

import Data.Foldable

import Quipper
import Quipper.Printing

import Classical
import Print

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
  , ("xnor.pdf"   , print_generic PDF xnor (qubit,qubit))
  ]

main :: IO ()
main = for_ examples (uncurry captureTo)
