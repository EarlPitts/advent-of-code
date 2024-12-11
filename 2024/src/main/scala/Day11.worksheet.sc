import util.*

import cats.*
import cats.implicits.*

val input = unsafeGetInputRaw.strip
  .split(" ")
  .map(_.toLong)
  .toList

def multiply(i: Long): List[Long] =
  val iStr = i.toString
  val len = iStr.length
  List(iStr.take(len / 2).toLong, iStr.drop(len / 2).toLong)

def step(i: Long): List[Long] =
  if i == 0L
  then List(1L)
  else if i.toString.length % 2 == 0 then multiply(i)
  else List(i * 2024L)

def merge(m1: Map[Long, Long], m2: Map[Long, Long]): Map[Long, Long] =
  m2.foldLeft(m1) { case (acc, (key, value)) =>
    acc.updated(key, acc.getOrElse(key, 0L) + value)
  }

def blink(m: Map[Long, Long]): Map[Long, Long] =
  m.flatMap { (num, count) =>
    step(num).map { n =>
      Map(n -> count * 1L)
    }
  }.foldLeft(Map())((m1: Map[Long, Long], m2: Map[Long, Long]) => merge(m1, m2))

def run(times: Int): Map[Long, Long] => Map[Long, Long] =
  util.monoidEndo.combineAll(List.fill(times)(blink))

val init = Map.from(input.zip(LazyList.continually(1L)))

// Part 1
run(25)(init).values.sum

// Part 2
run(75)(init).values.sum
