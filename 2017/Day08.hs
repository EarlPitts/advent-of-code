import Control.Monad.State qualified as ST
import Data.Either
import Data.List
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.String

data Operation = Inc | Dec deriving (Show, Eq)

type Register = String

data Rel
  = REQ
  | RNE
  | RLT
  | RGT
  | RGE
  | RLE
  deriving (Show, Eq)

data Condition
  = Cond Register Rel Int
  deriving (Show, Eq)

data Instruction = Instruction
  { reg :: Register,
    op :: Operation,
    val :: Int,
    cond :: Condition
  }
  deriving (Show, Eq)

p :: Parser [Instruction]
p = sepEndBy pLine spaces

pLine :: Parser Instruction
pLine = do
  r <- pReg
  space
  o <- pOp
  space
  v <- pVal
  space
  c <- pCond
  return $ Instruction r o v c

pReg :: Parser Register
pReg = many lower

pOp :: Parser Operation
pOp = fromStr <$> (string "inc" <|> string "dec")
  where
    fromStr s = if s == "inc" then Inc else Dec

pVal :: Parser Int
pVal = read <$> (minus <|> many1 digit)
  where
    minus = do
      char '-'
      n <- many1 digit
      return ("-" ++ n)

pCond = do
  string "if"
  space
  reg <- many lower
  space
  rel <- pRel
  space
  v <- pVal
  return $ Cond reg rel v

pRel :: Parser Rel
pRel =
  fromStr
    <$> choice
      [ try (string "<="),
        try (string ">="),
        string ">",
        string "<",
        string "!=",
        string "=="
      ]
  where
    fromStr "==" = REQ
    fromStr "!=" = RNE
    fromStr "<=" = RLE
    fromStr ">=" = RGE
    fromStr "<" = RLT
    fromStr ">" = RGT

type State = Register -> Int

initState :: Register -> Int
initState = const 0

showState :: [Register] -> State -> String
showState vars s =
  intercalate
    ", "
    [v ++ " = " ++ show (s v) | v <- vars]

update :: Register -> Int -> State -> State
update reg val s r
  | reg == r = val
  | otherwise = s r

readReg :: State -> Register -> Int
readReg = ($)

registers :: [Instruction] -> [Register]
registers = nub . fmap reg

run :: [Instruction] -> State -> State
run [] s = s
run (i : is) s = if checkCond i s then run is (execStep i s) else run is s

execStep :: Instruction -> State -> State
execStep (Instruction reg Dec val _) s = update reg (readReg s reg - val) s
execStep (Instruction reg Inc val _) s = update reg (readReg s reg + val) s

checkCond i s = case rel of
  REQ -> readReg s reg == val
  RNE -> readReg s reg /= val
  RLT -> readReg s reg < val
  RGT -> readReg s reg > val
  RGE -> readReg s reg >= val
  RLE -> readReg s reg <= val
  where
    (Cond reg rel val) = cond i

parseAndRun :: String -> String
parseAndRun s = show $ maximum $ sortOn fst $ fmap (\r -> (readReg endState r, r)) usedRegs
  where
    is = case parse p "" s of
      Right is' -> is'
      Left _ -> []
    usedRegs = registers is
    endState = run is initState

main = interact parseAndRun'

maxVal :: State -> [Register] -> Int
maxVal s rs = maximum (fmap (readReg s) rs)

run' :: [Instruction] -> State -> [Register] -> ST.State Int State
run' [] s rs = return s
run' (i : is) s rs =
  if checkCond i s
    then do
      m <- ST.get
      let curr = maxVal s rs
      if curr > m
        then do
          ST.put curr
          run' is (execStep i s) rs
        else run' is (execStep i s) rs
    else run' is s rs

parseAndRun' :: String -> String
parseAndRun' s = show maxVal
  where
    is = case parse p "" s of
      Right is' -> is'
      Left _ -> []
    usedRegs = registers is
    maxVal = ST.execState (run' is initState usedRegs) 0
