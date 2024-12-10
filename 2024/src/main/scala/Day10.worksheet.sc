import util._

import cats.*
import cats.implicits.*

val table = unsafeGetInputRaw
  .split("\n")
  .toList
  .map(_.toList.map(_.toString.toInt))

type Table = List[List[Int]]

case class Tile(row: Int, col: Int, level: Int)

def trailheads(table: Table): List[Tile] = for
  (r, ri) <- table.zipWithIndex
  (c, ci) <- r.zipWithIndex
  _ <- Alternative[List].guard(c == 0)
yield Tile(ri, ci, 0)

def neighbors(t: Table, p: Tile): List[Tile] =
  val r = p.row
  val c = p.col
  List((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1))
    .filter((x, y) => x >= 0 && x < t.size && y >= 0 && y < t.size)
    .map((x, y) => Tile(x, y, t(x)(y)))

def step(t: Table, p: Tile): List[Tile] =
  neighbors(t, p).filter(_.level == p.level + 1)

def run(t: Table, p: Tile): List[Tile] =
  if p.level == 9 then List(p)
  else step(t, p).map(run(t, _)).flatten

val ts = trailheads(table)

// Part 1
ts.map(run(table, _)).map(_.distinct.size).sum

// Part 2
ts.map(run(table, _)).map(_.size).sum
