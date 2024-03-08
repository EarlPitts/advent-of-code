package Day04

import cats.effect.{IO, IOApp}
import cats.syntax.all._
import cats.implicits._
import cats.Order
import java.time.LocalDateTime
import parsley.Parsley
import parsley.Success
import parsley.character.{digit, string, char, spaces, newline}
import parsley.combinator.{some, eof, sepEndBy}

import util.*
import java.time.temporal.ChronoUnit

case class Timestamp(dt: LocalDateTime)

type ID = Int

enum Event:
  case Sleep, Wake
  case Begin(id: ID)

case class Record(t: Timestamp, e: Event)

type Shift = List[Record]

lazy val p: Parsley[List[Record]] = sepEndBy(pRecord, newline) <* eof

lazy val pRecord: Parsley[Record] = for
  t <- pTimestamp
  _ <- spaces
  e <- pEvent
yield Record(t,e)

lazy val pTimestamp: Parsley[Timestamp] = for
  _ <- char('[')
  y <- pInt
  _ <- char('-')
  m <- pInt
  _ <- char('-')
  d <- pInt
  _ <- spaces
  h <- pInt
  _ <- char(':')
  min <- pInt
  _ <- char(']')
yield Timestamp(LocalDateTime.of(y,m,d,h,min))

lazy val pInt: Parsley[Int] = some(digit).map(_.mkString.toInt)

lazy val pEvent: Parsley[Event] = pBegin <|> pAsleep <|> pWake

lazy val pBegin = string("Guard #") *> pInt.map(Event.Begin.apply) <* string(" begins shift")
lazy val pAsleep = string("falls asleep") *> Parsley.pure(Event.Sleep)
lazy val pWake = string("wakes up") *> Parsley.pure(Event.Wake)

given Order[Record] with
  def compare(x: Record, y: Record): Int =
    (x,y) match
      case (Record(Timestamp(dt1),_), Record(Timestamp(dt2),_)) =>
        dt1.compareTo(dt2)

def intervals(s: Shift): List[List[Record]] = s.tail.grouped(2).toList

def id(s: Shift): ID =
  s match
    case Record(_, Event.Begin(id)) :: _ => id
    case _ => throw new RuntimeException("Shouldn't happen")

def byId(i: ID, shifts: List[Shift]): List[Shift] =
  shifts.filter(id(_) == i)

def length(rs: List[Record]): Long =
  rs match
    case List(Record(Timestamp(sleep),_), Record(Timestamp(wake),_)) =>
      ChronoUnit.MINUTES.between(sleep, wake);
    case _ => throw new RuntimeException("Shouldn't happen")

def lengths(shifts: List[Shift]): Map[ID, Long] =
  def shiftAsleep(rs: Shift): Long = intervals(rs).map(length).sum
  shifts.groupBy(id).mapValues(shifts => shifts.map(shiftAsleep).sum).toMap

def sleepsMost: List[Record] => ID =
  ((m: Map[Int, Long]) => m.maxBy(_._2)._1) compose lengths compose shifts

def minutes(rs: List[Record]): List[Int] =
  val len = length(rs)
  rs match
    case List(Record(Timestamp(sleep),_), _) =>
      (sleep.getMinute() until sleep.getMinute() + len.toInt).toList.map(n => n % 60)

def bestMinute(id: ID, shifts: List[Shift]): Int =
  byId(id, shifts).flatMap(intervals).flatMap(minutes).sorted.groupBy(identity).maxBy(_._2.size)._1

def secondDay(shifts: List[Shift]) =
  val (i: ID, (min: Int,_)) = shifts.filterNot(_.size == 1)
    .groupBy(id).mapValues(_.flatMap(intervals).flatMap(minutes)
    .groupBy(identity).maxBy(_._2.size).map(_.size)).maxBy(_._2._2)
  i * min

def shifts(rs: List[Record]): List[Shift] =
  def collect(rs: List[List[Record]], r: Record): List[Shift] =
    r match
      case Record(_, Event.Begin(_)) => List(r) :: rs
      case _                         => (rs.head :+ r) :: rs.tail
  rs.sorted.foldLeft(List())(collect)

object Main extends IOApp.Simple:
  def run: IO[Unit] = for
    data <- getInputRaw
    Success(rs) = p.parse(data)
    snoozy = sleepsMost(rs)
    _ <- IO.println(snoozy * bestMinute(snoozy,shifts(rs)))
    _ <- IO.println(secondDay(shifts(rs)))
  yield ()
