import util._

val (x, y) =
  unsafeGetInput("input.txt")
    .map(_.split(" +").map(c => c.toInt))
    .map { case Array(x, y) => (x, y) }
    .unzip

// Part 1
x.sorted.zip(y.sorted).map(_ - _).map(_.abs).sum

// Part 2
x.map(curr => curr * y.filter(_ == curr).size).sum
