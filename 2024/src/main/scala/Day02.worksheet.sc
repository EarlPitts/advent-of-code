import util._

val input =
  unsafeGetInput("input.txt")
    .map(_.split(" ").toList)
    .map(_.map(_.toInt))

def isSafe(report: List[Int]): Boolean =
  (report == report.sorted || report == report.sorted.reverse) &&
    adjPairs(report).forall { case (a, b) =>
      val diff = (a - b).abs
      diff >= 1 && diff <= 3
    }

def adjPairs[A](l: List[A]): List[(A, A)] =
  if l.size == 2
  then List((l.head, l.last))
  else (l.head, l.drop(1).head) :: adjPairs(l.drop(1))

input.filter { report =>
  isSafe(report) ||
  removeOne(report)
    .exists(isSafe)
}.size
