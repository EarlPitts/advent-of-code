package Day03

import cats.effect.{IO, IOApp}
import cats.syntax.all._
import cats.implicits._
import cats.{Eq, Order}
import cats.derived.*

import parsley.Success
import parsley.Parsley
import parsley.character.{digit, char, spaces, newline}
import parsley.combinator.{some, eof, sepEndBy}

import util.*

case class Claim(id: Int, left: Int, top: Int, width: Int, length: Int)

def p: Parsley[List[Claim]] = sepEndBy(pClaim, newline) <* eof

def pClaim: Parsley[Claim] = for
  _ <- char('#')
  id <- pInt
  _ <- spaces *> char('@') *> spaces
  l <- pInt
  _ <- char(',')
  t <- pInt
  _ <- char(':') *> spaces
  wid <- pInt
  _ <- char('x')
  len <- pInt
yield Claim(id, l, t, wid, len)

def pInt: Parsley[Int] = some(digit).map(_.mkString).map(_.toInt)

case class Coord(x: Int, y: Int) derives Eq, Order

def coords(width: Int, length: Int): List[Coord] = for
  x <- List.range(0,width)
  y <- List.range(0,length)
yield Coord(x,y)

def shift(l: Int, t: Int)(c: Coord): Coord =
  c match
    case Coord(x,y) => Coord(x+l,y+t)

def coordify(c: Claim): List[Coord] =
  c match
    case Claim(_, left, top, width, length) =>
      coords(width, length).map(shift(left, top))


object Main extends IOApp.Simple:
  def run: IO[Unit] = for
    data <- getInputRaw
    Success(claims) = p.parse(data)
    cs = claims.map(coordify)
    overlaps = group(cs.flatten).filter(_.size >= 2).map(_.head)
    _ <- IO.println(overlaps.size)
  yield ()
