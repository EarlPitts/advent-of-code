import util._

import cats.*
import cats.implicits.*
import scala.util.Try

import Dir.*

val input = unsafeGetInput("input.txt")
  .toArray
  .map(_.toCharArray)

val g = Grid(
  Zipper.fromList(input.toList.map((l: Array[Char]) => Zipper.fromList(l.toList)))
)

// ------------- Part 1 --------------

given monoidEndo[A]: Monoid[Endo[A]] with
  def combine(f: Endo[A], g: Endo[A]): Endo[A] = f compose g
  def empty: Endo[A] = identity

enum Dir:
  case Up, Down, Left, Right

  def turnRight: Dir = this match
    case Down  => Left
    case Up    => Right
    case Right => Down
    case Left  => Up

object Dir:
  def fromChar(c: Char): Dir = c match
    case '^' => Up
    case 'v' => Down
    case '>' => Right
    case '<' => Left

case class InitState(pos: Pos, dir: Dir)

val List(init) = for
  (r, ri) <- input.toList.zipWithIndex
  (c, ci) <- r.zipWithIndex
  _ <- Alternative[List].guard(c != '#' && c != '.')
yield InitState(Pos(ri, ci), Dir.fromChar(c))

def moveToPos(g: Grid[Char], pos: Pos): Grid[Char] =
  val moveDown = Monoid[Endo[Grid[Char]]]
    .combineAll(List.fill(pos.r)((g: Grid[Char]) => g.moveDown))
  val moveRight = Monoid[Endo[Grid[Char]]]
    .combineAll(List.fill(pos.c)((g: Grid[Char]) => g.moveRight))
  moveRight(moveDown(g))

def mark(g: Grid[Char]): Grid[Char] =
  g.update(_ => 'X')

def execute(pos: Grid[Char], dir: Dir): Grid[Char] =
  def step(
      f: Grid[Char] => Option[Grid[Char]],
      g: Grid[Char] => Grid[Char]
  ): Grid[Char] =
    f(pos) match
      case None => mark(pos)
      case Some(value) =>
        if value.extract == '#'
        then execute(pos, dir.turnRight)
        else execute(g(mark(pos)), dir)
  dir match
    case Down  => step(_.safeDown, _.moveDown)
    case Up    => step(_.safeUp, _.moveUp)
    case Right => step(_.safeRight, _.moveRight)
    case Left  => step(_.safeLeft, _.moveLeft)

execute(mark(moveToPos(g, init.pos)), init.dir).toLists
  .map(_.filter(_ == 'X'))
  .map(_.size)
  .sum

// ------------- Part 2 --------------

def alternatives(l: Array[Array[Char]]): List[Array[Array[Char]]] =
  val size = l.size - 1
  val coords = (0 to size).toList.flatMap(x => (0 to size).toList.map((x, _)))
  coords.map { (x, y) =>
    val newL = l.map(_.clone)
    if newL(x)(y) == '.' then { newL(x)(y) = '#'; newL }
    else l
  }

val alts = alternatives(input)

case class Pos(r: Int, c: Int)
type Table = Array[Array[Char]]

def getsOut(table: Table, pos: Pos, dir: Dir, fuel: Int): Boolean =
  if fuel <= 0 then false
  else
    dir match
      case Down =>
        Try(table(pos.r + 1)(pos.c)).toOption match
          case None => true
          case Some(value) =>
            if value == '#' then getsOut(table, pos, Left, fuel)
            else getsOut(table, Pos(pos.r + 1, pos.c), dir, fuel - 1)
      case Up =>
        Try(table(pos.r - 1)(pos.c)).toOption match
          case None => true
          case Some(value) =>
            if value == '#' then getsOut(table, pos, Right, fuel)
            else getsOut(table, Pos(pos.r - 1, pos.c), dir, fuel - 1)
      case Left =>
        Try(table(pos.r)(pos.c - 1)).toOption match
          case None => true
          case Some(value) =>
            if value == '#' then getsOut(table, pos, Up, fuel)
            else getsOut(table, Pos(pos.r, pos.c - 1), dir, fuel - 1)
      case Right =>
        Try(table(pos.r)(pos.c + 1)).toOption match
          case None => true
          case Some(value) =>
            if value == '#' then getsOut(table, pos, Down, fuel)
            else getsOut(table, Pos(pos.r, pos.c + 1), dir, fuel - 1)

alts
  .map { grid =>
    getsOut(grid, init.pos, init.dir, 10000)
  }
  .filter(_ == false)
  .size
