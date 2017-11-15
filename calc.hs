-- calc ["1", "+", "1"]
-- calc ["1", "-", "1"]
-- calc ["2", "*", "3"]
-- calc ["4", "/", "2"]

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show


calc l = calcTree (toTree l)
toTree l = Node (l !! 1) (Leaf (l !! 0)) (Leaf (l !! 2))

calcTree (Leaf a) = toNum a
calcTree (Node op a b) = (operation op) (calcTree a) (calcTree b)


toNum num = read num :: Int

operation s
  | s == "+" = (+)
  | s == "-" = (-)
  | s == "*" = (*)
  | s == "/" = div
  
  

--  ["1", "+", "30", "-", "2"] -> 29


-- calcTree (Node "+" (Leaf "3") (Leaf "2"))
-- calcTree (Node "-" (Node "+" (Leaf "1") (Leaf "30")) (Leaf "2"))

--  ["2", "-", "1", "*", "2", "+", "-1"] -> -1