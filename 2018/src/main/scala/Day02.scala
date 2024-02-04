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

def hasN[A : Eq : Order](n: Int)(l: List[A]): Boolean =
  group(l).map(_.size).contains(n)

def checksum(l: List[String]): Int =
  l.map(_.toList).filter(hasN(2)).size * l.map(_.toList).filter(hasN(3)).size

def differByOne[A: Eq](l1: List[A], l2: List[A]): Boolean =
  l1.zip(l2).map((a,b) => (a == b)).filter(_ == false).size == 1

def cross[A](l1: List[A], l2: List[A]): List[(A,A)] = l1.flatMap(a => l2.flatMap(b => List((a,b)))).distinct

def solve(l: List[String]): String =
  cross(l.map(_.toList),l.map(_.toList)).filter((a,b) => differByOne(a,b)).head match
    case (l1,l2) => l1.zip(l2).filter((a,b) => a == b).map(_._1).mkString

object Main extends IOApp.Simple:
  def run: IO[Unit] = for
    data <- getInput
    _ <- IO.println(checksum(data))
    _ <- IO.println(solve(data))
  yield ()
