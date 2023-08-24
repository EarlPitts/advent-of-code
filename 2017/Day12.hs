import Data.Graph
import Text.Parsec
import Text.Parsec.String
import Data.Either

type Node = Int

pLine :: Parser (String, Node, [Node])
pLine = do
  n <- many1 digit
  space
  string "<->"
  space
  ns <- (fmap . fmap) read (sepBy (many1 digit) (string ", "))
  newline
  return (n,read n,ns)

p :: Parser Graph
p = do
  lines <- many pLine
  let (graph,_,_) = graphFromEdges lines
  return graph

main :: IO ()
main = do
  input <- getContents
  let graph = fromRight (error "Oh no!") (parse p "" input)
  print (length $ reachable graph 0)
  print (length $ components graph)
