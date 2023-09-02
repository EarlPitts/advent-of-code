import Control.Monad.State as S
import Data.Maybe
import Debug.Trace
import Text.Parsec
import Text.Parsec.String

-- Syntax
data Expr
  = Freq Int
  | Reg Char
  deriving (Show, Eq)

data Instr
  = Snd Expr
  | Rec Expr
  | Set Expr Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Mod Expr Expr
  | Jmp Expr Expr
  deriving (Show, Eq)

-- Parsing
pReg :: Parser Expr
pReg = Reg <$> lower

pFreq :: Parser Expr
pFreq = Freq <$> pNum

pExpr :: Parser Expr
pExpr = pReg <|> pFreq

pNum :: Parser Int
pNum = read <$> (minus <|> many1 digit)
  where
    minus = do
      char '-'
      n <- many1 digit
      return ("-" ++ n)

pUnary :: Parser Instr
pUnary = do
  op <- getUnary <$> choice (string <$> ["rcv", "snd"])
  space
  e <- pExpr
  return $ op e
  where
    getUnary "snd" = Snd
    getUnary "rcv" = Rec

pBin :: Parser Instr
pBin = do
  op <- getBin <$> choice (try . string <$> ["set", "add", "mul", "mod", "jgz"])
  space
  e <- pExpr
  space
  e' <- pExpr
  return $ op e e'
  where
    getBin "set" = Set
    getBin "add" = Add
    getBin "mul" = Mul
    getBin "mod" = Mod
    getBin "jgz" = Jmp

p :: Parser [Instr]
p = sepEndBy (pBin <|> pUnary) newline

-- Dynamic semantics
type Name = Char

type Freq = Int

type Register = (Name, Freq)

data VM = VM
  { regs :: [Register],
    ip :: Int,
    lastSound :: Maybe Freq,
    recovered :: Freq
  }
  deriving (Show, Eq)

eval :: Instr -> VM -> VM
eval (Snd e) m = sound e m
eval (Rec e) m = rec e m
eval (Set e e') m = set e e' m
eval (Add e e') m = add e e' m
eval (Mul e e') m = mul e e' m
eval (Mod e e') m = modulo e e' m
eval (Jmp e e') m = jump e e' m

sound :: Expr -> VM -> VM
sound (Freq n) m = m {lastSound = Just n}
sound (Reg r) m = m {lastSound = Just (getFreq r m)}

set :: Expr -> Expr -> VM -> VM
set (Reg r) (Freq f) m = setFreq r f m
set (Reg r) (Reg r') m = setFreq r (getFreq r' m) m
set _ _ m = error "Oh no!"

add :: Expr -> Expr -> VM -> VM
add (Reg r) (Reg r') m = setFreq r (getFreq r m + getFreq r' m) m
add (Reg r) (Freq f) m = setFreq r (getFreq r m + f) m
add _ _ _ = error "Oh no!"

mul :: Expr -> Expr -> VM -> VM
mul (Reg r) (Reg r') m = setFreq r (getFreq r m * getFreq r' m) m
mul (Reg r) (Freq f) m = setFreq r (getFreq r m * f) m
mul _ _ _ = error "Oh no!"

modulo :: Expr -> Expr -> VM -> VM
modulo (Reg r) (Reg r') m = setFreq r (getFreq r m `mod` getFreq r' m) m
modulo (Reg r) (Freq f) m = setFreq r (getFreq r m `mod` f) m
modulo _ _ _ = error "Oh no!"

jump :: Expr -> Expr -> VM -> VM
jump (Reg r) (Reg r') m = if getFreq r m > 0 then m {ip = ip m + (getFreq r' m - 1)} else m
jump (Reg r) (Freq f) m = if getFreq r m > 0 then m {ip = ip m + (f - 1)} else m
jump (Freq f) (Reg r) m = if f > 0 then m {ip = ip m + (getFreq r m - 1)} else m
jump (Freq f) (Freq f') m = if f > 0 then m {ip = ip m + (f - 1)} else m

rec :: Expr -> VM -> VM
rec (Reg r) m = if getFreq r m /= 0 then m {recovered = s} else m
  where
    (Just s) = lastSound m
rec (Freq f) m = if f /= 0 then m {recovered = s} else m
  where
    (Just s) = lastSound m

incrIp :: VM -> VM
incrIp m = m {ip = succ (ip m)}

getFreq :: Name -> VM -> Freq
getFreq name (VM regs _ _ _) = case lookup name regs of
  (Just f) -> f
  Nothing -> 0

-- This is a little bit memory inefficient :)
setFreq :: Name -> Freq -> VM -> VM
setFreq n f m@(VM regs _ _ _) = m {regs = (n, f) : regs}

run :: [Instr] -> S.State VM Freq
run is = do
  s <- get
  let rec = recovered s
  if notElem (ip s) [0 .. length is - 1] || rec /= 0
    then return rec
    else do
      let i = is !! ip s
      let newState = eval i s
      put $ incrIp newState
      run is

type Val = Int

data Queue = Queue [Val] [Val] deriving (Show, Eq)

emptyQueue :: Queue
emptyQueue = Queue [] []

isEmpty :: Queue -> Bool
isEmpty = (== emptyQueue)

enqueue :: Val -> Queue -> Queue
enqueue v (Queue h t) = Queue h (v : t)

dequeue :: Queue -> (Val, Queue)
dequeue (Queue [] t) = dequeue (Queue (reverse t) [])
dequeue (Queue (h : hs) t) = (h, Queue hs t)

data VM' = VM'
  { regs' :: [Register],
    ip' :: Int,
    received :: Queue,
    sent :: Maybe Val,
    blocked :: Bool
  }
  deriving (Show, Eq)

eval' :: Instr -> VM' -> VM'
eval' (Snd e) m = send e m
eval' (Rec e) m = receive e m
eval' (Set e e') m = set' e e' m
eval' (Add e e') m = add' e e' m
eval' (Mul e e') m = mul' e e' m
eval' (Mod e e') m = modulo' e e' m
eval' (Jmp e e') m = jump' e e' m

run' :: [Instr] -> S.State (VM', VM', Val) Val
run' is = do
  (s, s', cnt) <- get
  let states = (s, s')
  let newCnt = if isJust (sent s) then succ cnt else cnt
  let (s, s') = exchangeMessages states
  if (blocking s && blocking s') || notElem (ip' s) [0 .. length is - 1] || notElem (ip' s') [0 .. length is - 1]
    then return newCnt
    else do
      let i = is !! ip' s
      let i' = is !! ip' s'
      let newState = eval' i s
      let newState' = eval' i' s'
      put (incrIp' newState, incrIp' newState', newCnt)
      run' is

exchangeMessages :: (VM', VM') -> (VM', VM')
exchangeMessages (s, s') = case (sent s, sent s') of
  (Just v, Just v') ->
    ( s {received = enqueue v' (received s), sent = Nothing},
      s' {received = enqueue v (received s'), sent = Nothing}
    )
  (Nothing, Just v') ->
    ( s {received = enqueue v' (received s)},
      s' {sent = Nothing}
    )
  (Just v, Nothing) ->
    ( s {sent = Nothing},
      s' {received = enqueue v (received s')}
    )
  _ -> (s, s')

send :: Expr -> VM' -> VM'
send (Freq n) m = m {sent = Just n}
send (Reg r) m = m {sent = Just (getReg r m)}

set' :: Expr -> Expr -> VM' -> VM'
set' (Reg r) (Freq f) m = setReg r f m
set' (Reg r) (Reg r') m = setReg r (getReg r' m) m
set' _ _ m = error "Oh no!"

add' :: Expr -> Expr -> VM' -> VM'
add' (Reg r) (Reg r') m = setReg r (getReg r m + getReg r' m) m
add' (Reg r) (Freq f) m = setReg r (getReg r m + f) m
add' _ _ _ = error "Oh no!"

mul' :: Expr -> Expr -> VM' -> VM'
mul' (Reg r) (Reg r') m = setReg r (getReg r m * getReg r' m) m
mul' (Reg r) (Freq f) m = setReg r (getReg r m * f) m
mul' _ _ _ = error "Oh no!"

modulo' :: Expr -> Expr -> VM' -> VM'
modulo' (Reg r) (Reg r') m = setReg r (getReg r m `mod` getReg r' m) m
modulo' (Reg r) (Freq f) m = setReg r (getReg r m `mod` f) m
modulo' _ _ _ = error "Oh no!"

jump' :: Expr -> Expr -> VM' -> VM'
jump' (Reg r) (Reg r') m = if getReg r m > 0 then m {ip' = ip' m + (getReg r' m - 1)} else m
jump' (Reg r) (Freq f) m = if getReg r m > 0 then m {ip' = ip' m + (f - 1)} else m
jump' (Freq f) (Reg r) m = if f > 0 then m {ip' = ip' m + (getReg r m - 1)} else m
jump' (Freq f) (Freq f') m = if f > 0 then m {ip' = ip' m + (f - 1)} else m

receive :: Expr -> VM' -> VM'
receive (Reg r) m = if isEmpty (received m) then m {blocked = True} else (setReg r v m) {received = q}
  where
    (v, q) = dequeue (received m)

incrIp' :: VM' -> VM'
incrIp' m
  | blocking m = m
  | otherwise = m {ip' = succ (ip' m)}

getReg :: Name -> VM' -> Val
getReg r (VM' rs _ _ _ _) = case lookup r rs of
  (Just v) -> v
  Nothing -> 0

setReg :: Name -> Val -> VM' -> VM'
setReg n f m@(VM' rs _ _ _ _) = m {regs' = (n, f) : rs}

blocking :: VM' -> Bool
blocking (VM' _ _ _ _ b) = b

main :: IO ()
main = do
  input <- getContents
  let (Right is) = parse p "" input
  let initState = VM [] 0 Nothing 0
  -- print $ evalState (run is) initState
  print $ evalState (run' is) ((VM' [('p', 0)] 0 emptyQueue Nothing False), VM' [('p', 1)] 0 emptyQueue Nothing False, 0)
