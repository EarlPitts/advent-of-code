package Day09

import cats._
import cats.implicits._
import cats.effect._

case class Zipper(l: List[Int], m: Int, r: List[Int]):
  def moveLeft(steps: Int): Zipper =
    val len = (l.length + 1 + r.length)
    val s = steps % len
    if s === 0 then return this
    if s > l.length
    then moveRight(len - s)
    else Zipper(l.drop(s), l(s - 1), l.take(s - 1).reverse ::: m :: r)

  def moveRight(steps: Int): Zipper =
    val len = (l.length + 1 + r.length)
    val s = steps % len
    if s === 0 then return this
    if s > r.length
    then moveLeft(len - s)
    else Zipper(r.take(s - 1).reverse ::: m :: l, r(s - 1), r.drop(s))

  def add(n: Int): Zipper =
    Zipper(m :: l, n, r)

  def removeCurrent: (Zipper, Int) =
    val cur = m
    val newZipper = Zipper(l, r.head, r.tail)
    (newZipper, cur)

  def toList: List[Int | String] =
    l.reverse ::: List("(", m, ")") ::: r


object Zipper:
  def apply(i: Int): Zipper =
    Zipper(List(), i, List())

  def apply(is: List[Int], cur: Int): Zipper =
    val l = is.take(cur).reverse
    val m = is(cur)
    val r = is.drop(cur + 1)
    Zipper(l, m, r)

type State = (Zipper, Players)
type Players = List[Int]
case class Game(players: Int, steps: Int)

def step(t: State, m: Int): State = t match
  case (z,p) =>
    if m % 23 === 0
    then
      val ind = m % p.length
      val (newZ, removed) = z.moveLeft(7).removeCurrent
      val newP = p.updated(ind, p(ind) + m + removed)
      (newZ, newP)
    else (z.moveRight(1).add(m),p)

def runGame(g: Game): State = g match
  case Game(p,s) =>
    val players = (0 until p).toList.as(0)
    (1 to s).toList.foldl((Zipper(0),players))(step)

object Main extends IOApp.Simple:
  val g = Game(429, 7090100)
  def run: IO[Unit] = IO.println(runGame(g)._2.max)
