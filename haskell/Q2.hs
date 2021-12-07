module Q2 where

import Q1

to_rna :: [Nucl] -> [Nucl]
to_rna [] = []
to_rna (x:xs)
  | x == T = U : to_rna xs
  | otherwise = x : to_rna xs

test_to_rna :: Bool
test_to_rna = to_rna (s2n "GATGGAACTTGACTACGTAAATT") == s2n "GAUGGAACUUGACUACGUAAAUU"
