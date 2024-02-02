package Day02

import cats.effect.{IO, IOApp}
import cats.syntax.all._
import cats.implicits._
import cats.{Eq, Order}

import util.getInput

def group[A: Eq : Order](l: List[A]): List[List[A]] =
  def go(acc: List[List[A]], curr: List[A], rest: List[A]): List[List[A]] =
    (curr,rest) match
      case (_,Nil) => curr :: acc
      case (curr, a :: as) => if curr.contains(a)
                              then go(acc, a :: curr, as)
                              else go(curr :: acc, List(a), as)
  val a :: as = l.sorted
  go(List(), List(a), as)

def checksum(l: List[String]): Int = ???
  // l.map(count).sum

object Main extends IOApp.Simple:
  def run: IO[Unit] = for
    data <- getInput
    _ <- data.traverse(IO.println(_))
  yield ()
