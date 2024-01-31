package Day01

import cats.effect.{IO, IOApp}
import cats.syntax.all._

import scala.collection.immutable.LazyList

import util.getInput

def findDup[A](l: LazyList[A]): A =
  def go(seen: Set[A], rest: LazyList[A]): A =
    rest match
      case (h #:: t) => if seen.contains(h) then h else go(seen + h, t)
  go(Set(), l)

object Main extends IOApp.Simple:
  def run: IO[Unit] = for
    data <- getInput.map(_.map(_.toInt))
    sum = data.foldRight(0)(_ + _)
    sums = LazyList.continually(data).flatten.scanLeft(0)(_ + _)

    _ <- IO.println(sum)
    _ <- IO.println(findDup(sums))
  yield ()
