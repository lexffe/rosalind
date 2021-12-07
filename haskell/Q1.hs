module Q1 where

data Nucl = A | C | T | G | U
type Strand = [Nucl]

instance Show Nucl where
  show A = "A"
  show C = "C"
  show T = "T"
  show G = "G"
  show U = "U"

instance Eq Nucl where
  (==) A A = True
  (==) C C = True
  (==) T T = True
  (==) G G = True
  (==) U U = True
  (==) _ _ = False

s2n ::  String -> Strand
s2n "" = []
s2n (x:xs)
  | x == 'A' = A : s2n xs
  | x == 'C' = C : s2n xs
  | x == 'T' = T : s2n xs
  | x == 'G' = G : s2n xs
  | x == 'U' = U : s2n xs
  | otherwise = []

fn :: Strand -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
fn [] (a, c, t, g) = (a, c, t, g)
fn (n:ns) (a,c,t,g)
  | n == A = fn ns (a+1, c, t, g)
  | n == C = fn ns (a, c+1, t, g)
  | n == T = fn ns (a, c, t+1, g)
  | n == G = fn ns (a, c, t, g+1)
fn _ _ = (0, 0, 0, 0)
