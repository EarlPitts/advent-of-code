import util._

import cats._
import cats.implicits._

val lines = unsafeGetInput("input.txt").map(_.strip).toList

//------------------- Part 1 -------------------

def words(size: Int)(g: Grid[Char]): List[String] =
  def move(f: Grid[Char] => Option[Grid[Char]]): String =
    List
      .iterate(g.pure[Option], size)(_.flatMap(f))
      .map(_.map(_.extract))
      .collect { case Some(x) => x }
      .mkString

  List(
    move(_.safeRight), // E
    move(_.safeLeft), // W
    move(_.safeDown), // S
    move(_.safeUp), // N
    move(_.safeRight.flatMap(_.safeUp)), // NE
    move(_.safeRight.flatMap(_.safeDown)), // SE
    move(_.safeLeft.flatMap(_.safeUp)), // NW
    move(_.safeLeft.flatMap(_.safeDown)) // SW
  )

val g = Grid(
  Zipper.fromList(lines.map((s: String) => Zipper.fromList(s.toList)))
)

def numOfXmas(g: Grid[Char]): Int =
  if g.extract == 'X' then words(4)(g).filter(_ == "XMAS").size
  else 0

val init1 = g.coflatMap(numOfXmas)

List
  .iterate(init1, 140)(_.moveRight)
  .flatMap(List.iterate(_, 140)(_.moveDown))
  .map(_.extract)
  .sum

//------------------- Part 2 -------------------

def corners[A](grid: Grid[A]): List[A] = List(
  grid.moveUp.moveLeft,
  grid.moveUp.moveRight,
  grid.moveDown.moveLeft,
  grid.moveDown.moveRight
).map(_.extract)

def isXmas(g: Grid[Char]): Boolean =
  if g.extract == 'A' then
    corners(g) match
      case List(a, b, c, d) =>
        (List(a, d).mkString == "MS" ||
          List(d, a).mkString == "MS") &&
        (List(b, c).mkString == "MS" ||
          List(c, b).mkString == "MS")
      case _ => false
  else false

val init2 = g.moveDown.moveRight.coflatMap(isXmas)

List
  .iterate(init2, 138)(_.moveRight)
  .flatMap(List.iterate(_, 138)(_.moveDown))
  .map(_.extract)
  .filter(_ == true)
  .size
