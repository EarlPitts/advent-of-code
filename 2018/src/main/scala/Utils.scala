package util

import scala.io.Source
import cats.effect.IO

val fileName = "input.txt"

def getInput: IO[List[String]] =
  IO.blocking(Source.fromFile(fileName).getLines().toList)
