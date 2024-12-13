import util.*

import Button.*
import parsley.Success
import parsley.Parsley
import parsley.Parsley.{many, some, atomic}
import parsley.character.*
import parsley.combinator.*

val input = unsafeGetInputRaw

def p: Parsley[List[(Button, Button, Prize)]] =
  sepEndBy(pTriple, newline)

def pInt: Parsley[Int] =
  some(digit)
    .map(_.mkString)
    .map(_.toInt)

def pTriple: Parsley[(Button, Button, Prize)] = for
  a <- pButton
  b <- pButton
  p <- pPrize
yield (a, b, p)

def pButton: Parsley[Button] = for
  _ <- many(noneOf('+')) *> char('+')
  x <- pInt
  _ <- many(noneOf('+')) *> char('+')
  y <- pInt
yield Button(x, y)

def pPrize: Parsley[Prize] = for
  _ <- many(noneOf('=')) *> char('=')
  x <- pInt
  _ <- many(noneOf('=')) *> char('=')
  y <- pInt
yield Prize(x, y)

case class Button(x: Int, y: Int)
case class Prize(x: Int, y: Int)

val Success(in) = p.parse(input)

// Part 1
val nums = for
  x <- (0 to 100)
  y <- (0 to 100)
yield (x, y)

in.map { case (Button(ax, ay), Button(bx, by), Prize(px, py)) =>
  nums.filter { (a, b) =>
    (((a * ax) + (b * bx)) == px &&
    ((a * ay) + (b * by)) == py)
  }
}.map {
  case Vector((a: Int, b: Int)) => 3 * a + b
  case Vector()                 => 0
}.sum
