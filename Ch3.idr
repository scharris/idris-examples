import Data.Vect
import Data.Nat

insert : Ord el => el -> Vect len el -> Vect (S len) el
insert x [] = [x]
insert x l@(s :: ss) =
  if x <= s then
    x :: l
  else
    s :: insert x ss

insSort : Ord el => Vect n el -> Vect n el
insSort [] = []
insSort (x :: xs) = 
  let xsSorted = insSort xs in
      insert x xsSorted

rev : List a -> List a
rev xs = reverseIter xs []
  where
    reverseIter : List a -> List a -> List a
    reverseIter [] acc = acc
    reverseIter (y::ys) acc = reverseIter ys (y::acc)

vlen : {n : Nat} -> Vect n a -> Nat
vlen v = n

{-
reverseIter : Vect k1 a -> Vect k2 a -> Vect (k1 + k2) a
reverseIter acc [] = rewrite plusZeroRightNeutral k1 in acc
reverseIter acc (x :: xs) = reverseIter (x :: acc) xs

Error: While processing right hand side of reverseIter. Can't solve constraint between: k1 and plus k1 0.
...
reverseIter acc [] = acc
                     ^^^
-}

createEmpties : {n : Nat} -> Vect n (Vect 0 a)
createEmpties {n = 0} = []
createEmpties {n = (S k)} = [] :: createEmpties


-- matrix transpose
transposeMat : {n : Nat} -> Vect m (Vect n e) -> Vect n (Vect m e)
transposeMat [] = replicate n []
transposeMat (r :: rs) = prependCol r (transposeMat rs)
  where
    prependCol : {n : Nat} -> Vect n e -> Vect n (Vect k e) -> Vect n (Vect (S k) e)
    prependCol [] [] = []
    prependCol (x :: xs) (row :: rows) = (x :: row) :: prependCol xs rows

-- matrix transpose implemented with zipWith
transposeMatz : {n,m : Nat} -> Vect m (Vect n e) -> Vect n (Vect m e)
transposeMatz [] = replicate n []
transposeMatz (r :: rs) = zipWith (\el,trRow => el :: trRow) r (transposeMatz rs)


addMat : Num e => {n : Nat} -> Vect m (Vect n e) -> Vect m (Vect n e) -> Vect m (Vect n e)
addMat m1 m2 = zipWith (\r1,r2 => addRows r1 r2) m1 m2
  where
    addRows: Vect n e -> Vect n e -> Vect n e
    addRows x y = zipWith (+) x y

addMatz : Num e => Vect m (Vect n e) -> Vect m (Vect n e) -> Vect m (Vect n e)
addMatz m1 m2 = zipWith (\r1,r2 => zipWith (+) r1 r2) m1 m2


mulMat : Num e => {m,k,n : Nat} -> Vect m (Vect k e) -> Vect k (Vect n e) -> Vect m (Vect n e)
mulMat [] _ = []
mulMat ([] :: xs) []  = replicate m (replicate n 0) -- a bit arbitrary but this and above case aren't covered by mathematical definition of mat mult anyway
mulMat m1 m2 = map (\r => lcomb r m2) m1
  where
    addVecs : {k : Nat} -> Vect k (Vect n e) -> Vect n e
    addVecs [] = replicate n 0
    addVecs (x :: xs) = zipWith (+) x (addVecs xs)

    svmult : e -> Vect n e -> Vect n e
    svmult c v = map (c *) v

    lcomb : Vect k e -> Vect k (Vect n e) -> Vect n e
    lcomb cs vs = addVecs (zipWith svmult cs vs)

