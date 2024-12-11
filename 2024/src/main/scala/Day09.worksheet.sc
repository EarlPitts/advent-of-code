import util.*

import cats.*
import cats.implicits.*

import Block.*
import Segment.*

// val input = "2333133121414131402".toList.map(_.toString.toInt)
val input = unsafeGetInputRaw.strip.toList.map(_.toString.toInt)

enum Block:
  case Free
  case Filled(id: ID)

type ID = Int

def f(nums: List[Int]): List[Block] =
  val n = LazyList.iterate(true)(!_).zip(input).zipWithIndex
  n.foldl(List()) { case (l, ((b, i), id)) =>
    if b then List.fill(i)(Filled(id / 2)) ++ l else List.fill(i)(Free) ++ l
  }

val blocks = f(input).reverse

// Part 1

def g(bs: List[Block]): Option[(List[Block], ID)] =
  val ize = bs.reverse.dropWhile {
    case Free => true; case Filled(_) => false
  }
  ize.headOption match
    case Some(Filled(id)) => Some((ize.tail.reverse, id))
    case None             => None

def defrag(bs: List[Block]): List[Block] =
  def go(acc: List[Block], bs: List[Block]): List[Block] = bs match
    case Nil              => acc
    case Filled(id) :: bs => go(Filled(id) :: acc, bs)
    case Free :: bs =>
      g(bs) match
        case Some((rem, id)) => go(Filled(id) :: acc, rem)
        case None            => acc
  go(Nil, bs).reverse

def checksum(bs: List[Block]): Long =
  bs.zipWithIndex.foldLeft(0L) { case (acc, (b, i)) =>
    b match
      case Filled(id) => acc + id.toLong * i.toLong
      case Free       => acc
  }

checksum(defrag(blocks))

// Part 2

enum Segment:
  case File(id: Int, size: Int)
  case FreeSpace(size: Int)

def segmentate(bs: List[Block]): List[Segment] =
  bs.foldLeft(List()) {
    case (FreeSpace(size) :: ss, Free) => FreeSpace(size + 1) :: ss
    case (FreeSpace(size) :: ss, Filled(id)) =>
      File(id, 1) :: (FreeSpace(size) :: ss)
    case (File(id, size) :: ss, Free) => FreeSpace(1) :: (File(id, size) :: ss)
    case (File(id1, size) :: ss, Filled(id2)) =>
      if id1 == id2 then File(id1, size + 1) :: ss
      else File(id2, 1) :: (File(id1, size) :: ss)
    case (Nil, Free)       => List(FreeSpace(1))
    case (Nil, Filled(id)) => List(File(id, 1))
  }


def dropFile(files: List[Segment], f: File): List[Segment] =
  files
    .foldLeft(List.empty[Segment]) {
      case (acc, x) if x == f => FreeSpace(f.size) :: acc
      case (acc, x)           => x :: acc
    }
    .reverse

def paste(segments: List[Segment], file: File): Option[List[Segment]] =
  def go(
      acc: Option[List[Segment]],
      segments: List[Segment]
  ): Option[List[Segment]] = segments match
    case Nil                    => None
    case (f @ File(_, _)) :: xs => go(acc.map(f :: _), xs)
    case FreeSpace(size) :: xs =>
      if size >= file.size
      then
        if size == file.size
        then acc.map((ss: List[Segment]) => xs.reverse ++ (file :: ss))
        else acc.map((ss: List[Segment]) => xs.reverse ++ (FreeSpace(size - file.size) :: (file :: ss)))
      else go(acc.map(FreeSpace(size) :: _), xs)
  go(Some(List.empty[Segment]), segments).map(_.reverse)

def newDefrag(spaces: List[Segment]): List[Segment] =
  def go(spaces: List[Segment], files: List[Segment]): List[Segment] =
    files match
      case Nil => spaces
      case (f @ File(_, _)) :: fs =>
        val (leftSide, rightSide) = spaces.span(_ != f)
        paste(leftSide, f) match
          case None => go(spaces, fs)
          case Some(spaces) =>
            go(spaces ++ dropFile(rightSide, f), fs)
  go(
    spaces,
    spaces.filter {
      case File(_, _) => true; case FreeSpace(_) => false
    }.reverse
  )

def listify(spaces: List[Segment]): List[Block] =
  spaces.foldLeft(List.empty[Block]) {
    case (acc, File(id, size))  => acc ++ List.fill(size)(Filled(id))
    case (acc, FreeSpace(size)) => acc ++ List.fill(size)(Free)
  }

// checksum(listify(newDefrag(segmentate(blocks).reverse)))
