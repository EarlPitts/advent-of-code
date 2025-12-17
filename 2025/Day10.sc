//> using dep com.github.vagmcs::optimus:3.4.5
//> using dep com.github.vagmcs::optimus-solver-oj:3.4.5
//> using dep com.github.j-mie6::parsley:4.6.1

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPIntVar
import optimus.algebra.Int2Const
import optimus.algebra.Const
import optimus.algebra.Expression
import parsley.Parsley
import parsley.Success
import parsley.character.*
import parsley.combinator.*

import scala.io.Source

case class Machine(
    lights: Array[Int],
    buttons: List[Array[Int]],
    joltage: Array[Int]
)

def p: Parsley[List[Machine]] = sepEndBy(pMachine, newline)

val input = Source.fromFile("input.txt").mkString

def pLights: Parsley[Array[Int]] = for
  _ <- char('[')
  lights <- many(choice(char('.'), char('#')))
  _ <- string("] ")
yield lights
  .map(c => if c == '.' then 0 else 1)
  .toArray

def pButtons = sepEndBy(pButton, space)

def pMachine = for
  ls <- pLights
  bs <- pButtons
  j <- pJoltage
yield Machine(ls, bs.map(toVector(j.length)), j)

def toVector(len: Int)(button: Array[Int]): Array[Int] =
  var arr = Array.fill(len)(0)
  for (idx <- button) {
    arr(idx) = 1
  }
  arr

def pInt: Parsley[Int] =
  some(digit)
    .map(_.mkString)
    .map(_.toInt)

def pButton =
  (char('(') *> sepBy(pInt, (char(','))) <* char(')'))
    .map(_.toArray)

def pJoltage =
  (char('{') *> sepBy(pInt, char(',')) <* char('}'))
    .map(_.toArray)

def solve: Machine => Double =
  case Machine(_, bs, target) =>
    implicit val model: MPModel = MPModel(SolverLib.oJSolver)
    val vars = List.fill(bs.length)(MPIntVar(0 to 1000))
    for (i <- target.indices) {
      add(
        bs.zip(vars).foldRight[Expression](Const(0)) { case ((vec, v), acc) =>
          v * vec(i) + acc
        } := target(i)
      )
    }

    minimize(vars.reduce(_ + _))

    start()
    val res = objectiveValue
    release()
    res

def solution: List[Machine] => Long = _.map(solve).sum.round

println(p.parse(input).map(solution).get)
