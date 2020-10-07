module Bfs where

data Flup a = Flup (Flup a) (Flup a)

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

collect :: Tree a -> [a] -> [a]
collect Leaf xs         = xs
collect (Node x _ _) xs = xs ++ [x]

bfs :: Tree a -> (Tree a -> b -> b) -> b -> b
bfs t f x = (fst . (bfs' f)) (x, [t])

bfs' :: (Tree a -> b -> b) -> (b, [Tree a]) -> (b, [Tree a])
bfs' _ (x, [])     = (x, [])
bfs' f (x, (t:ts)) = case t of
  Leaf              -> bfs' f (f t x, ts)
  Node _ left right -> bfs' f (f t x, ts ++ [left, right])






ones = 1:ones
blah (x:xs) ys = x : zipWith (+) xs ys
foo = blah ones foo


blah2 :





sieve :: [Integer] -> [Integer]
sieve (n:ns) = n : ((sieve . filter (\x -> x `mod` n /= 0)) ns)

primes = sieve [2..]

type NextNode a = Either a (Tree a)

nextNode :: Eq a => Tree a -> NextNode a -> NextNode a
nextNode Leaf _ = Right Leaf
nextNode t@(Node x _ _) query = case query of
  Left n -> if n == x then Right t else Left n
  Right r -> Right r

label :: [Int] -> Tree Int -> Tree Int
label ns t = label' ns (nextNode t)

label' :: [Int] -> (NextNode Int -> NextNode Int) -> Tree Int
label' (n:ns) nodes = case nodes (Left (n - 1)) of
  Left  _    -> undefined
  Right node -> case node of
    Leaf                -> Leaf
    (Node _ left right) -> label' ns newRight
      where
        newLeft = (nodes . nextNode left)
        newRight = (newLeft . nextNode right)

chain1 = nextNode (Node 1 Leaf Leaf) . nextNode (Node 2 Leaf Leaf) . nextNode (Node 3 Leaf Leaf)

t1 :: Tree Int
t1 = Leaf

t2 :: Tree Int
t2 = Node 100 Leaf Leaf

t3 :: Tree Int
t3 = Node 100 (Node 200 Leaf Leaf) Leaf

t4 :: Tree Int
t4 = Node 100 (Node 200 Leaf Leaf) (Node 300 Leaf Leaf)

t5 :: Tree Int
t5 = Node 100 (Node 200 (Node 400 Leaf Leaf)
                        (Node 500 Leaf Leaf))
              (Node 300 Leaf Leaf)
