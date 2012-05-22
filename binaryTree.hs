----
------ BINÁRNÍ STROM
-------------------------

data BTree a 
  = Empty 
  | Node a (BTree a) (BTree a)
  deriving (Show, Eq)

-- prida listy 
singleton :: a -> BTree a
singleton x = Node x (Empty) (Empty)

-- vlozeni noveho nodu = vetve
treeInsert :: (Ord a) => a -> BTree a -> BTree a
treeInsert x Empty = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right) 
-- treeInsert 1 (Node 0 (Empty) (Empty))


treeFromList :: (Ord a) => [a] -> BTree a
treeFromList xs = foldl (\acc x -> treeInsert x acc) Empty xs

  
treeElem :: (Ord a) => a -> BTree a -> Bool
treeElem x Empty = False
treeElem x (Node a left right) 
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- je prazdny ?  
treeEmpty :: BTree a -> Bool
treeEmpty Empty = True
treeEmpty _ = False
-- pouziti: treeEmpty Empty == True
-- pouziti: treeEmpty Node 0 (Empty) (Empty) == False

-- je nyni neprazdny ?
treeNotEmpty :: BTree a -> Bool
treeNotEmpty Empty = False
treeNotEmpty _ = True
-- treeNotEmpty (Node 0 (Empty) (Empty))

-- odstraneni vetve
treeRemoveElem :: (Ord a) => a -> BTree a -> BTree a
treeRemoveElem x Empty = Empty
treeRemoveElem x (Node a left right)
  | x < a = Node a (treeRemoveElem x left) right
  | x > a = Node a left (treeRemoveElem x right)
  | x == a && treeEmpty left && treeEmpty right = Empty
  | x == a && treeEmpty left && treeNotEmpty right = right
  | x == a && treeNotEmpty left && treeEmpty right = left
  | otherwise = Node (mostLeft right) left (treeRemoveElem (mostLeft right) right)
      where mostLeft (Node b l r) = if treeEmpty l then b else mostLeft l
-- treeRemoveElem 0 (Node 0 (Empty) (Empty))


treePathToElem :: (Ord a) => a -> BTree a -> [a]
treePathToElem x tree 
  | not (treeElem x tree) = []
  | otherwise = path x tree []
      where path x (Node a left right) l
              | x == a = l ++ [a]
              | x < a = path x left (l ++ [a])
              | x > a = path x right (l ++ [a])
-- treePathToElem 2 (Node 0 Empty (Node 1 Empty (Node 2 Empty (Node 3 Empty Empty)))) == [0,1,2]

treeDepth :: BTree a -> Int
treeDepth Empty = 0
treeDepth (Node a left right) = 1 + max (treeDepth left) (treeDepth right)

treePreorder :: BTree a -> [a]
treePreorder x = pre x
  where
    pre Empty = []
    pre (Node a left right) = (a:(pre left)) ++ (pre right) 
      
treePostorder :: BTree a -> [a]
treePostorder x = post x
  where 
    post Empty = []
    post (Node a left right) = ((post left) ++ (post right)) ++ [a] 

treeInorder :: BTree a -> [a]
treeInorder x = ino x 
  where 
    ino Empty = []
    ino (Node a left right) = (ino left ) ++ (a:(ino right))    

-- treePreorder   (Node 0 Empty (Node 1 Empty (Node 2 Empty (Node 3 Empty Empty))))
-- [0,1,2,3]
-- treePostorder  (Node 0 Empty (Node 1 Empty (Node 2 Empty (Node 3 Empty Empty))))
-- [3,2,1,0]
-- treeInorder    (Node 0 Empty (Node 1 Empty (Node 2 Empty (Node 3 Empty Empty))))
-- [0,1,2,3]

   
