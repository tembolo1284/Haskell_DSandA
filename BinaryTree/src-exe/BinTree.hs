data BinTree a = Empty | Node a (BinTree a) (BinTree a)
  deriving (Show)
t7 :: BinTree Int
t7 = Node 7 Empty Empty

t6 :: BinTree Int
t6 = Node 6 Empty Empty

t5 :: BinTree Int
t5 = Node 5 Empty Empty

t4 :: BinTree Int
t4 = Node 4 Empty Empty

t3 :: BinTree Int
t3 = Node 3 t6 t7

t2 :: BinTree Int
t2 = Node 2 t4 t5

t1 :: BinTree Int
t1 = Node 1 t2 t3

t1' :: BinTree Int
t1' = Node 1 t3 t2

size :: BinTree a -> Int
size Empty = 0
size (Node _ lc rc) = 1 + size (lc) + size (rc)

height :: BinTree a -> Int
height Empty = 0
height (Node _ lc rc) = 1 + max (height lc) (height rc)
