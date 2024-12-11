import util._

import cats.*
import cats.implicits.*

object Day10 extends App:
  val input =
    "0 7 198844 5687836 58 2478 25475 894"
      .split(" ")
      .map(_.toString.toLong)
      .toList

  def multiply(i: Long): List[Long] =
    val iStr = i.toString
    val len = iStr.length
    List(iStr.take(len/2).toLong, iStr.drop(len/2).toLong)

  def step(l: List[Long]): List[Long] =
    l.foldLeft(Nil) { (acc, curr) =>
      if curr == 0L
      then acc :+ 1L
      else if curr.toString.length % 2 == 0 then acc ++ multiply(curr)
      else acc :+ curr * 2024L
    }

  def run(input: List[Long], times: Int): List[Long] =
    if times == 0 then input
    else println(times);run(step(input), times - 1)

  println(run(input, 25).size)
