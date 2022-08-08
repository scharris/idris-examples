module Average

import Data.String
import Data.List
import Data.Vect

export
||| Compute average number of characters per word in the input string.
average: String -> Double
average str =
  let strWords = words str
      numWords = length strWords
      totalLengths = sum (map length strWords)
   in cast totalLengths / cast numWords

myreverse : List a -> List a
myreverse xs = revIter xs []
  where
    revIter : List a -> List a -> List a
    revIter [] ys = ys
    revIter (x :: xs) ys = revIter xs (x :: ys)

mylength : List a -> Nat
mylength [] = 0
mylength (x :: xs) = 1 + mylength xs

mymap : (f : a -> b) -> List a -> List b
mymap f [] = []
mymap f (x :: xs) = f x :: mymap f xs

vmap : (f : a -> b) -> Vect n a -> Vect n b
vmap f [] = []
vmap f (x :: xs) = f x :: vmap f xs

vecTake : (k : Fin (S n)) -> Vect n el -> Vect (cast k) el
vecTake FZ v = []
vecTake (FS k) (x :: xs) = x :: vecTake k xs

sumEntries : Num a => {n: Nat} -> (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos u v = case (integerToFin pos n) of
  Nothing => Nothing
  Just i => Just $ (index i u) + (index i v)

sumEntries2 : Num a => {n: Nat} -> (pos : Nat) -> Vect n a -> Vect n a -> Maybe a
sumEntries2 pos u v = case (natToFin pos n) of
  Nothing => Nothing
  Just i => Just $ (index i u) + (index i v)

sumEntries3 : Num a => {n: Nat} -> (pos : Fin n) -> Vect n a -> Vect n a -> a
sumEntries3 pos u v = (index pos u) + (index pos v)

mySumVects : Num a => Vect n a -> Vect n a -> a
mySumVects [] [] = 0
mySumVects (x :: xs) (y :: ys) = x + y + mySumVects xs ys
