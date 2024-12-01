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

def group[A: Eq : Order](l: List[A]): List[List[A]] =
  def go(acc: List[List[A]], curr: List[A], rest: List[A]): List[List[A]] =
    (curr,rest) match
      case (_,Nil) => curr :: acc
      case (curr, a :: as) => if curr.contains(a)
                              then go(acc, a :: curr, as)
                              else go(curr :: acc, List(a), as)
  val a :: as = l.sorted
  go(List(), List(a), as)
