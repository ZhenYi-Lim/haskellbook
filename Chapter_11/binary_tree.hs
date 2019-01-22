data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) f a (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder Node left a right = x ++ y ++ z
    where
        x = [a]
        y = preorder left
        z = preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder Node left a right = x ++ y ++ z
    where
        x = inorder left
        y = [a]
        z = inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder Node left a right = x ++ y ++ z
    where
        x = postorder left
        y = postorder right
        z = [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left x right) =
    (foldTree f (f x (foldTree f z left)) right) --foldr pattern of passing default value inward