package Day05

import cats.effect.{IO, IOApp}
import cats.syntax.all._
import cats.implicits._

import util._

def step(s: List[Char]): List[Char] =
  def f(x: Char, xs: List[Char]): List[Char] = xs match
    case Nil => List(x)
    case y :: xs => if x.toLower == y.toLower && x != y
                    then xs
                    else x :: y :: xs
  s.foldRight(Nil)(f)

def eval(polymer: List[Char]): List[Char] =
  val newPolymer = step(polymer)
  if newPolymer == polymer then polymer else eval(newPolymer)
  
object Main extends IOApp.Simple:
  def run: IO[Unit] = for
    data <- getInputRaw.map(_.strip.toCharArray.toList)
    _ <- IO.println(eval(data).size)
  yield ()
