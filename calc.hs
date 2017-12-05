data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show


calc tree = calcTree $ toTree tree

toTree tree = toTree' $ reverse tree


toTree' (a:[]) = Leaf a
toTree' (a:op:rest) = Node op (toTree' rest) (Leaf a)

calcTree (Leaf a) = toNum a
calcTree (Node op a b) = (operation op) (calcTree a) (calcTree b)


toNum num = read num :: Float

operation s
  | s == "+" = (+)
  | s == "-" = (-)
  | s == "*" = (*)
  | s == "/" = (/)
