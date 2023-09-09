import Text.Parsec
import Text.Parsec.String
import Data.List

data Particle = Particle Position Velocity Acceleration deriving (Show,Eq)
data Position = Position Int Int Int deriving (Show,Eq)
data Velocity = Velocity Int Int Int deriving (Show,Eq)
data Acceleration = Acceleration Int Int Int deriving (Show,Eq)

-- Parser
p :: Parser [Particle]
p = sepEndBy pPart newline 

pPart :: Parser Particle
pPart = do
  p <- pPosition
  string ", "
  v <- pVelocity
  string ", "
  a <- pAcceleration
  return $ Particle p v a

pNeg :: Parser Int
pNeg = do
  try (char '-')
  num <- many1 digit
  return $ read ('-':num)

pPos :: Parser Int
pPos = do
  num <- many1 digit
  return $ read num

pInt :: Parser Int
pInt = pNeg <|> pPos

pTriplet :: Parser (Int,Int,Int)
pTriplet = do
  x <- pInt
  char ','
  y <- pInt
  char ','
  z <- pInt
  return (x,y,z)

uncurry3 :: (a -> a -> a -> b) -> (a,a,a) -> b
uncurry3 f (a,b,c) = f a b c

pPosition :: Parser Position
pPosition = string "p=<" *> (uncurry3 Position <$> pTriplet) <* char '>'

pVelocity :: Parser Velocity
pVelocity = string "v=<" *> (uncurry3 Velocity <$> pTriplet) <* char '>'

pAcceleration :: Parser Acceleration
pAcceleration = string "a=<" *> (uncurry3 Acceleration <$> pTriplet) <* char '>'

-- Simulation
step :: Particle -> Particle
step (Particle p v a) = Particle newPos newVel a
  where newVel = incrVel v a
        newPos = movePos p newVel

incrVel :: Velocity -> Acceleration -> Velocity
incrVel (Velocity x y z) (Acceleration x' y' z') = Velocity (x + x') (y + y') (z + z')

movePos :: Position -> Velocity -> Position
movePos (Position x y z) (Velocity x' y' z') = Position (x + x') (y + y') (z + z')

dist :: Particle -> Int
dist (Particle (Position x y z) _ _) = abs x + abs y + abs z

main :: IO ()
main = do
  input <- getContents
  let (Right ps) = parse p "" input
  let distances = fmap dist . iterate step <$> ps
  let longRun = (!! 1000) <$> distances
  let closest = head . sortOn snd $ zip [0..] longRun
  print closest
