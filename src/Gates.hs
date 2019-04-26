module Gates(reverseQ) where
import Quipper

-- | Reverses a list of qubits
reverseQ :: [Qubit] -> Circ [Qubit]
reverseQ = return . reverse

-- | Swaps two qubits
swap :: Int -> Int -> [Qubit] -> Circ [Qubit]
swap i j = return . swapL i j
 where
  swapL i j xs =
    let (l1, li:resto) = splitAt i xs
        (l2, lj:l3   ) = splitAt j resto
    in  l1 ++ [lj] ++ l2 ++ [li] ++ l3
