import util._

import parsley.Success
import parsley.Parsley
import parsley.Parsley.{many, some, atomic}
import parsley.character._
import parsley.combinator._

sealed trait Statement
case object Do extends Statement
case object Dont extends Statement
sealed trait Expr extends Statement
case class Mul(i1: Int, i2: Int) extends Expr

def p: Parsley[List[Statement]] =
  many(choice(pDo, pDont, pMul) <|> item)
    .map(_.collect { case s: Statement => s })

def pInt: Parsley[Int] = some(digit).map(_.mkString).map(_.toInt)

def pMul: Parsley[Expr] = atomic(for
  _ <- string("mul(")
  x <- pInt
  _ <- char(',')
  y <- pInt
  _ <- char(')')
yield Mul(x, y))

def pDo: Parsley[Statement] = atomic(string("do()")).as(Do)
def pDont: Parsley[Statement] = atomic(string("don't()")).as(Dont)

def eval(e: Expr): Int = e match
  case Mul(i1, i2) => i1 * i2

def execute(doit: Boolean, ss: List[Statement]): Int = ss match
  case s :: ss =>
    s match
      case e: Expr =>
        if doit then eval(e) + execute(doit, ss) else execute(doit, ss)
      case Do   => execute(true, ss)
      case Dont => execute(false, ss)
  case List() => 0

val statements = p.parse(unsafeGetInputRaw).get

execute(true, statements)
