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
  | x == a && treeEmpty left && treeNotEmpty right = right -- jestli ze je leva strana prazdna a prave ne tak vracim pravou
  | x == a && treeNotEmpty left && treeEmpty right = left -- jestli je leva neprazdna a prava prazdna tak vracim levou
  | otherwise = Node (mostLeft right) left (treeRemoveElem (mostLeft right) right) -- pro oba dva neprazdne
      where mostLeft (Node b l r) = if treeEmpty l then b else mostLeft l
-- treeRemoveElem 0 (Node 0 (Empty) (Empty))
-- treeRemoveElem (-2) (Node 0 (Node (-1) (Node (-2) Empty Empty) Empty) (Node 1 Empty (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))))

--        0
--     /    \
--   -1       1
--   / \     / \
-- -2  E    E  2
-- /\         / \
-- E E       3  4
--              / \
--             E  E


-- poziti: Hlavni_strom heldany_strom = vysledny_strom
treeChangeValue :: (Ord a, Num a) => BTree a -> BTree a -> BTree a
treeChangeValue Empty Empty = Empty
treeChangeValue Empty _ = Empty -- protoze neexistuje hlavni strom
treeChangeValue mainTree Empty =  mainTree
treeChangeValue (Node a al ar) (Node b bl br)
  | b < a = Node a (treeChangeValue al b_all) ar -- jdi do leva
  | b > a = Node a al (treeChangeValue ar b_all) -- jdi do prava
  | a == b && treeEmpty bl && treeEmpty br = treeUpValue (Node a al ar)
  | a == b && treeNotEmpty bl && treeEmpty br = (Node newValue (treeChangeValue al bl) ar)
  | a == b && treeEmpty bl && treeNotEmpty br = (Node newValue al (treeChangeValue ar br))
  | a == b && treeNotEmpty bl && treeNotEmpty br = (Node newValue (treeChangeValue al bl) (treeChangeValue ar br))
--   | a == b && treeNotEmpty al && treeNotEmpty ar && treeNotEmpty bl && treeEmpty br  = (Node newValue (treeChangeValue al bl) (treeUpValue ar))
--   | a == b && treeNotEmpty al && treeNotEmpty ar && treeEmpty bl && treeNotEmpty br  = (Node newValue (treeUpValue al) (treeChangeValue ar br))
  where
    b_all = (Node b bl br)
--     a_all = (Node a al ar)
    newValue = if a >= 0 then a + 1 else a - 1
   
   
--- TESTY   
-- treeChangeValue (Node 0 (Node (-1) ((Node (-2) (Node (-3) Empty Empty) (Node (-4) Empty Empty))) Empty) (Node 1 Empty Empty)) (Node (-2) (Node (-3) Empty Empty) (Node (-4) Empty Empty))
   
-- treeChangeValue (Node 0 (Node (-1) Empty Empty) (Node 1 Empty Empty)) (Node 0 (Node (-1) Empty Empty) (Node 1 Empty Empty))

-- treeChangeValue (Node 0 (Node (-1) ((Node (-2) Empty (Node (-4) Empty Empty))) Empty) Empty)          (Node 0  Empty Empty)  
-- treeChangeValue (Node 0 (Node (-1) (Node (-2) Empty Empty) Empty) Empty)                              (Node (-1) (Node (-2) Empty Empty) Empty)

-- treeChangeValue (Node 0 (Node (-1) ((Node (-2) (Node (-3) Empty Empty) (Node (-4) Empty Empty))) Empty) (Node 1 Empty Empty))      (Node 0 Empty Empty)
-- treeChangeValue (Node 0 (Node (-1) ((Node (-2) (Node (-3) Empty Empty) (Node (-4) Empty Empty))) Empty) (Node 1 Empty Empty))      (Node 1 Empty Empty)    
-- treeChangeValue (Node 0 (Node (-1) ((Node (-2) (Node (-3) Empty Empty) (Node (-4) Empty Empty))) Empty) (Node 1 Empty Empty))      (Node 0 Empty (Node 1 Empty Empty))
--         0
--      /    \
--    -1       1
--    / \     / \
--  -2  E    E  E
--  / \        
-- -3 -4      
 
treeUpValue :: (Ord a, Num a) => BTree a -> BTree a
treeUpValue (Node a left right)
  | treeEmpty (Node a left right) = Empty
  | treeEmpty left && treeEmpty right = (Node newValue left right)
  | treeEmpty left && treeNotEmpty right = (Node newValue left (treeUpValue right))
  | treeNotEmpty left && treeEmpty right = (Node newValue (treeUpValue left) right)
  | otherwise = (Node newValue (treeUpValue left) (treeUpValue right)) -- pruchod preorder kdyz oba pravky nejsou prazdne
  where
    newValue = if a >= 0 then a + 1 else a - 1


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

   
-- treePreorder (Node 0 (Node (-1) (Node (-2) Empty Empty) Empty) (Node 1 Empty (Node 2 Empty (Node 3 Empty Empty))))
