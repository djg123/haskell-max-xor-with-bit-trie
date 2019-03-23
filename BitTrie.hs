import Data.Bits
import Data.List (tails)


data Node = LeafNode Int | BranchNode Node Node | Nil deriving (Show)


depths :: Node -> [Int]
depths (LeafNode _) = [0]
depths Nil = []
depths (BranchNode l r) = map (+1) (depths l ++ depths r)


retrieveValues :: Node -> [[Int]]
retrieveValues (LeafNode _) = [[]]
retrieveValues Nil = []
retrieveValues (BranchNode l r) = map (0:) (retrieveValues l) ++ map (1:) (retrieveValues r)


emptyTrie :: Node
emptyTrie = Nil


insertValue :: Node -> Int -> Node
insertValue node x = insertValue' node masks x


insertValues :: [Int] -> Node
insertValues = foldr (flip insertValue) Nil


insertValue' :: Node -> [Int] -> Int -> Node
insertValue' _ [] val = LeafNode val
insertValue' Nil (mask:remainingMasks) val = case val .&. mask == 0 of
  True -> BranchNode (insertValue' Nil remainingMasks val) Nil
  False -> BranchNode Nil (insertValue' Nil remainingMasks val)
insertValue' (LeafNode x) _ val = undefined
insertValue' (BranchNode l r) (mask:remainingMasks) val = case val .&. mask == 0 of
  True -> BranchNode (insertValue' l remainingMasks val) r
  False -> BranchNode l (insertValue' r remainingMasks val)


masks :: [Int]
masks = map (shiftL 1) [0..31]


maxXor :: [Int] -> Int
maxXor vals = maxXor' node node
  where
    node = foldr (flip insertValue) Nil vals


maxXor' :: Node -> Node -> Int
maxXor' (LeafNode x) (LeafNode y) = x `xor` y
maxXor' (BranchNode ll lr) (BranchNode rl rr) = case (ll, rr) of
  (x@(BranchNode _ _), y@(BranchNode _ _)) -> maxXor' x y
  ((LeafNode x), (LeafNode y)) -> x `xor` y
  (_, _)  -> case (lr, rl) of
    (x@(BranchNode _ _), y@(BranchNode _ _)) -> maxXor' x y
    ((LeafNode x), (LeafNode y)) -> x `xor` y
    (_, _) -> case (ll, rl) of
      (x@(BranchNode _ _), y@(BranchNode _ _)) -> maxXor' x y
      ((LeafNode x), (LeafNode y)) -> x `xor` y
      (_, _) -> case (lr, rr) of
        (x@(BranchNode _ _), y@(BranchNode _ _)) -> maxXor' x y
        ((LeafNode x), (LeafNode y)) -> x `xor` y



bruteForceMaxXor :: [Int] -> Int
bruteForceMaxXor xs = foldl max 0 $ map (\(a, b) -> a `xor` b) pairs
  where
    pairs :: [(Int, Int)]
    pairs = [(val, r) | (val, rest) <- zip xs (tails $ tail xs), r <- rest]
