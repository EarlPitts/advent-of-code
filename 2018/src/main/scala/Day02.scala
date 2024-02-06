package Day02

import cats.effect.{IO, IOApp}
import cats.syntax.all._
import cats.implicits._
import cats.{Eq, Order}

import util.*

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
