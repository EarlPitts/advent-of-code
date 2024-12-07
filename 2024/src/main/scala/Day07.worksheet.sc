import util._

val input = unsafeGetInput("input.txt").map(parseLine)

case class Data(res: Long, operands: List[Long])

def parseLine(line: String): Data =
  val Array(res, ops) = line.split(": ")
  Data(res.toLong, ops.split(" ").map(_.toLong).toList)

val operators: List[(Long, Long) => Long] = List(
  (_ + _),
  (_ * _),
  (x: Long, y: Long) => ((y.toString + x.toString).toLong)
)

def correct(d: Data): Boolean =
  combs(operators, d.operands.size - 1)
    .map { operators =>
      eval(d.operands.reverse, operators) == d.res
    }
    .contains(true)

def eval(operands: List[Long], operators: List[(Long, Long) => Long]): Long =
  operands match
    case List(o) => o
    case o :: os => operators.head(o, eval(os, operators.tail))

input
  .filter(correct)
  .map(_.res)
  .sum
