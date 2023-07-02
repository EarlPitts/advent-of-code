import Prelude hiding (Left, Right)

data Sign = Negative | Positive deriving (Show, Eq)
data Dir = Left | Right

-- Zipper for representing the jump instructions
data Zipper a = Zipper [a] a [a] deriving Eq

instance Show a => Show (Zipper a) where
  show (Zipper h c t) = show (reverse h) ++ " [" ++ show c ++ "] " ++ show t

mkZipper :: [a] -> Zipper a
mkZipper (a:as) = Zipper [] a as

getValue :: Zipper a -> a
getValue (Zipper _ c _) = c

moveLeft :: Zipper a -> Zipper a
moveLeft (Zipper (h:hs) c t) = Zipper hs h (c:t)

moveRight :: Zipper a -> Zipper a
moveRight (Zipper h c (t:ts)) = Zipper (c:h) t ts

modify :: Zipper a -> a -> Zipper a
modify (Zipper h c t) c' = Zipper h c' t

leftmost :: Zipper a -> Bool
leftmost (Zipper [] _ _) = True
leftmost _               = False

rightmost :: Zipper a -> Bool
rightmost (Zipper _ _ []) = True
rightmost _               = False

move :: Dir -> Int -> Zipper a -> Maybe (Zipper a)
move _ 0 z = Just z
move Left n z =
  if leftmost z
  then Nothing
  else move Left (n - 1) (moveLeft z)
move Right n z =
  if rightmost z
  then Nothing
  else move Right (n - 1) (moveRight z)

sign :: Int -> Sign
sign n = if n >= 0 then Positive else Negative

solve :: Int -> Zipper Int -> Int
solve n z = case move dir (abs val) new of
          Nothing -> n
          Just next -> solve (n + 1) next
  where val = getValue z
        dir = if sign val == Positive then Right else Left
        new = modify z (val + 1)

step :: Zipper Int -> Maybe (Zipper Int)
step z = move dir (abs val) new
  where val = getValue z
        dir = if sign val == Positive then Right else Left
        new = modify z (val + 1)

solution :: String -> String
solution = show . solve 1 . mkZipper . fmap read . lines

main = interact solution
