package Day08

import cats._
import cats.implicits._
import cats.effect._

import parsley.Success
import parsley.Parsley
import parsley.character.{digit, char, spaces, newline}
import parsley.combinator.{some, eof, sepEndBy, exactly}

import util._ 

case class Tree(metadata: List[Int], childNodes: List[Tree]):
  def toList: List[Int] =
    metadata ++ childNodes.foldl(List())((l: List[Int], t: Tree) => l ++ t.toList)

def pInt: Parsley[Int] = some(digit).map(_.mkString).map(_.toInt)

def p: Parsley[Tree] = for {
  childNum <- pInt
  _ <- spaces
  metadataNum <- pInt
  _ <- spaces
  children <- exactly(childNum, p)
  _ <- spaces
  metadata <- exactly(metadataNum, pInt <* spaces)
} yield Tree(metadata, children)

def getValue(tree: Tree): Int =
  def f(l: List[Option[Int]]): List[Int] =
    l.foldl(List())((a: List[Int], b: Option[Int]) => a :+ b.getOrElse(0))
  if tree.childNodes.length === 0
  then tree.metadata.sum
  else f(tree.metadata.map((i: Int) => tree.childNodes.get(i - 1)).map(_.map(getValue(_)))).sum


object Main extends IOApp.Simple:
  def run: IO[Unit] = for {
    data <- getInputRaw
    Success(t) = p.parse(data)
    _ <- IO.println(t.toList.sum)
    _ <- IO.println(getValue(t))
  } yield ()

