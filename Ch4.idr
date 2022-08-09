module Ch4

data Direction = North | East  | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle b h) = 0.5 * b * h
area (Rectangle w h) = w * h 
area (Circle r) = pi * r * r

data Tree e = Empty | Node (Tree e) e (Tree e)

insert : Ord e => e -> Tree e -> Tree e
insert x Empty = Node Empty x Empty
insert x orig@(Node left y right) =
  case compare x y of
    LT => Node (insert x left) y right
    GT => Node left y (insert x right)
    EQ => orig

-- Similar to above but putting Ord constraint in the data type constructors.
data BSTree : Type -> Type where
  Empty' : Ord e => BSTree e
  Node' : Ord e => (left : BSTree e) -> e -> (right : BSTree e) ->  BSTree e

insert' : e -> BSTree e -> BSTree e
insert' x Empty' = Node' Empty' x Empty'
insert' x orig@(Node' left val right) =
  case compare x val of
    LT => Node' (insert' x left) val right
    GT => Node' left val (insert' x right)
    EQ => orig


listToTree : Ord e => List e -> Tree e
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree e -> List e
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ (val :: treeToList right)

data IExpr
  = Lit Int  
  | Add IExpr IExpr
  | Sub IExpr IExpr
  | Mul IExpr IExpr

eval : IExpr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing x = x
maxMaybe j1@(Just x) Nothing = j1
maxMaybe j1@(Just x) j2@(Just y) = if x > y then j1 else j2


