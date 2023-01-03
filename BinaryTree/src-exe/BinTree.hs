module BinTree where

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
t1' = Node 1 t2 t3

t1'' :: BinTree Int
t1'' = Node 1 t3 t2

size :: BinTree a -> Int
size Empty = 0
size (Node _ lc rc) = 1 + size (lc) + size (rc)

height :: BinTree a -> Int
height Empty = 0
height (Node _ lc rc) = 1 + max (height lc) (height rc)

equal :: Eq a => BinTree a -> BinTree a -> Bool
equal Empty Empty = True
equal _ Empty = False
equal Empty _ = False
equal (Node x lx rx) (Node y ly ry)
    | x /= y = False
    | otherwise = equal lx ly && equal rx ry

isomorphic :: Eq a => BinTree a -> BinTree a -> Bool
isomorphic Empty Empty = True
isomorphic _ Empty = False
isomorphic Empty _ = False
isomorphic (Node x lx rx) (Node y ly ry)
    | x /= y = False
    | otherwise = (isomorphic lx ly && isomorphic rx ry) ||
                  (isomorphic lx ry && isomorphic rx ly)

--preorder traversal
preOrder :: BinTree a -> [a]
preOrder Empty = []
preOrder (Node x lc rc) = [x] ++ preOrder lc ++ preOrder rc

--postorder traversal
postOrder :: BinTree a -> [a]
postOrder Empty = []
postOrder (Node x lc rc) = postOrder lc ++ postOrder rc ++ [x]

--inorder traversal
inOrder :: BinTree a -> [a]
inOrder Empty = []
inOrder (Node x lc rc) = inOrder lc ++ [x] ++ inOrder rc

--Breadth First Search -- traversal by levels
breadthFirst :: BinTree a -> [a]
breadthFirst t = bfs [t]

bfs :: [BinTree a] -> [a]
bfs [] = []
bfs (Empty:xs) = bfs xs
bfs ((Node x lc rc):xs) = x : (bfs $ xs ++ [lc, rc]) 
