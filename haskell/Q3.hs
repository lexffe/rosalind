module Q3 where

import Q1
-- import Q2

comp :: Strand -> Strand
comp [] = []
comp (n:ns)
  | n == A = T : comp ns
  | n == T = A : comp ns
  | n == G = C : comp ns
  | n == C = G : comp ns
  | n == U = U : comp ns

rev :: Strand -> Strand
rev = reverse
