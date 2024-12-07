package util

import scala.io.Source
import cats.effect.IO
import cats._
import cats.implicits._

val fileName = "input.txt"

def unsafeGetInputRaw: String =
  Source.fromFile(fileName).mkString

def unsafeGetInput(fileName: String): List[String] =
  Source.fromFile(fileName).getLines.toList

def getInput: IO[List[String]] =
  IO.blocking(Source.fromFile(fileName).getLines().toList)

def getInputRaw: IO[String] =
  IO.blocking(Source.fromFile(fileName).mkString)

def group[A: Eq: Order](l: List[A]): List[List[A]] =
  def go(acc: List[List[A]], curr: List[A], rest: List[A]): List[List[A]] =
    (curr, rest) match
      case (_, Nil) => curr :: acc
      case (curr, a :: as) =>
        if curr.contains(a)
        then go(acc, a :: curr, as)
        else go(curr :: acc, List(a), as)
  val a :: as = l.sorted
  go(List(), List(a), as)

def removeOne[A](l: List[A]): List[List[A]] =
  (0 to l.size - 1).map { i =>
    l.take(i) ++ l.drop(i + 1)
  }.toList

def combs[A](list: List[A], n: Int): List[List[A]] =
  if (n == 0) List(List())
  else
    list.flatMap { elem =>
      combs(list, n - 1)
        .map(elem :: _)
    }

case class Zipper[A](left: LazyList[A], focus: A, right: LazyList[A]):
  def moveLeft: Zipper[A] =
    if (left.isEmpty) this
    else Zipper[A](left.tail, left.head, focus #:: right)

  def moveRight: Zipper[A] =
    if (right.isEmpty) this
    else Zipper[A](focus #:: left, right.head, right.tail)

  def safeLeft: Option[Zipper[A]] =
    if (left.isEmpty) None
    else Some(Zipper[A](left.tail, left.head, focus #:: right))

  def safeRight: Option[Zipper[A]] =
    if (right.isEmpty) None
    else Some(Zipper[A](focus #:: left, right.head, right.tail))

  private lazy val lefts: LazyList[Zipper[A]] =
    LazyList.iterate(this)(_.moveLeft).tail.zip(left).map(_._1)

  private lazy val rights: LazyList[Zipper[A]] =
    LazyList.iterate(this)(_.moveRight).zip(right).map(_._1)

  def map[B](f: A => B): Zipper[B] =
    Zipper[B](left.map(f), f(focus), right.map(f))

  lazy val duplicate: Zipper[Zipper[A]] =
    Zipper[Zipper[A]](lefts, this, rights)

  def extend[B](f: Zipper[A] => B): Zipper[B] =
    duplicate.map(f)

  def force: Zipper[A] =
    Zipper(left.force, focus, right.force)

  def toList: List[A] = left.toList.reverse ++ List(focus) ++ right.toList

  override def toString: String =
    s"|${left.reverse} >$focus< ${right.force}|"

object Zipper:
  def apply[A](left: List[A], f: A, right: List[A]): Zipper[A] =
    Zipper[A](left.reverse.to(LazyList), f, right.to(LazyList))

  def fromList[A](items: List[A]): Zipper[A] =
    Zipper(List(), items.head, items.tail)

  given Comonad[Zipper] = new Comonad[Zipper]:
    def extract[A](fa: Zipper[A]): A =
      fa.focus

    def coflatMap[A, B](fa: Zipper[A])(f: Zipper[A] => B): Zipper[B] =
      fa.extend(f)

    def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] =
      fa.map(f)

case class Grid[A](value: Zipper[Zipper[A]]):
  def map[B](f: A => B): Grid[B] =
    Grid(value.map(_.map(f)))

  def moveUp: Grid[A] =
    Grid(value.moveLeft)

  def moveDown: Grid[A] =
    Grid(value.moveRight)

  def moveLeft: Grid[A] =
    Grid(value.map(_.moveLeft))

  def moveRight: Grid[A] =
    Grid(value.map(_.moveRight))

  def safeUp: Option[Grid[A]] =
    value.safeLeft.map(Grid(_))

  def safeDown: Option[Grid[A]] =
    value.safeRight.map(Grid(_))

  def safeLeft: Option[Grid[A]] =
    value.extract.safeLeft match
      case None => None
      case Some(zipper) => Some(Grid(value.map(_.moveLeft)))

  def safeRight: Option[Grid[A]] =
    value.extract.safeRight match
      case None => None
      case Some(zipper) => Some(Grid(value.map(_.moveRight)))

  def extract: A =
    value.extract.extract

  def duplicate: Grid[Grid[A]] =
    def layer[X](u: Zipper[Zipper[X]]): Zipper[Zipper[Zipper[X]]] = {
      val lefts = LazyList
        .iterate(u)(ssx => ssx.map(_.moveLeft))
        .tail
        .zip(u.left)
        .map(_._1)
      val rights = LazyList
        .iterate(u)(ssx => ssx.map(_.moveRight))
        .tail
        .zip(u.right)
        .map(_._1)
      Zipper(lefts, u, rights)
    }

    val layers: Zipper[Zipper[Zipper[Zipper[A]]]] = layer(layer(value))
    Grid(layers).map(Grid.apply)

  def toLists: List[List[A]] =
    value.map(_.toList).toList

object Grid:
  given Comonad[Grid] = new Comonad[Grid]:
    def extract[A](fa: Grid[A]): A =
      fa.extract

    def coflatMap[A, B](fa: Grid[A])(f: Grid[A] => B): Grid[B] =
      map(fa.duplicate)(f)

    def map[A, B](fa: Grid[A])(f: A => B): Grid[B] =
      fa.map(f)
