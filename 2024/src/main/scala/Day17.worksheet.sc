import util.*

import cats.*
import cats.implicits.*
import parsley.*
import parsley.Success
import parsley.Parsley
import parsley.Parsley.{many, some, atomic}
import parsley.character.*
import parsley.combinator.*
import Instruction.*
import cats.data.State

enum Instruction:
  case Adv(combo: Int)
  case Bxl(lit: Int)
  case Bst(combo: Int)
  case Jnz(lit: Int)
  case Bxc
  case Out(combo: Int)
  case Bdv(combo: Int)
  case Cdv(combo: Int)

case class Registers(a: Int, b: Int, c: Int)

case class VMState(
    regs: Registers,
    ip: Option[Zipper[Instruction]],
    out: List[Int]
)

def pInt: Parsley[Int] =
  some(digit)
    .map(_.mkString)
    .map(_.toInt)

def parser: Parsley[(Registers, List[Instruction])] = for
  regs <- pRegisters
  _ <- newline *> newline
  prog <- pProgram
yield (regs, prog)

def pProgram: Parsley[List[Instruction]] =
  string("Program: ") *>
    sepBy(digit, char(','))
      .map(_.grouped(2).toList.map(pInstruction))

def pInstruction: List[Char] => Instruction =
  case '0' :: op => Adv(op.mkString.toInt)
  case '1' :: op => Bxl(op.mkString.toInt)
  case '2' :: op => Bst(op.mkString.toInt)
  case '3' :: op => Jnz(op.mkString.toInt)
  case '4' :: _  => Bxc
  case '5' :: op => Out(op.mkString.toInt)
  case '6' :: op => Bdv(op.mkString.toInt)
  case '7' :: op => Cdv(op.mkString.toInt)

def pRegisters: Parsley[Registers] = for
  a <- string("Register A: ") *> pInt
  _ <- newline
  b <- string("Register B: ") *> pInt
  _ <- newline
  c <- string("Register C: ") *> pInt
yield Registers(a, b, c)

def resolve(op: Int): State[VMState, Int] =
  if List.range(0,4).contains(op)
  then op.pure
  else
    op match
      case 4 => State.inspect(_.regs.a)
      case 5 => State.inspect(_.regs.b)
      case 6 => State.inspect(_.regs.c)

def advance: State[VMState, Unit] =
  State.modify[VMState](s => s.copy(ip = s.ip.flatMap(_.safeRight)))

def adv(combo: Int): State[VMState, Unit] = for
  numerator <- State.get[VMState].map(_.regs.a)
  comboVal <- resolve(combo)
  denominator = Math.pow(2, comboVal).toInt
  res = numerator / denominator
  _ <- State.modify[VMState](s => s.copy(regs = s.regs.copy(a = res)))
  _ <- advance
yield ()

def bxl(lit: Int): State[VMState, Unit] = for
  reg <- State.get[VMState].map(_.regs.b)
  _ <- State.modify[VMState](s => s.copy(regs = s.regs.copy(b = reg ^ lit)))
  _ <- advance
yield ()

def bst(combo: Int): State[VMState, Unit] = for
  res <- resolve(combo).map(_ % 8)
  _ <- State.modify[VMState](s => s.copy(regs = s.regs.copy(b = res)))
  _ <- advance
yield ()

def jnz(lit: Int): State[VMState, Unit] = for
  reg <- State.get[VMState].map(_.regs.a)
  _ <-
    if reg == 0
    then advance
    else State.modify[VMState](s => s.copy(ip = s.ip.flatMap(_.jump(lit))))
yield ()

def bxc: State[VMState, Unit] = for
  regB <- State.get[VMState].map(_.regs.b)
  regC <- State.get[VMState].map(_.regs.c)
  _ <- State.modify[VMState](s => s.copy(regs = s.regs.copy(b = regB ^ regC)))
  _ <- advance
yield ()

def out(combo: Int): State[VMState, Unit] = for
  res <- resolve(combo).map(_ % 8)
  _ <- State.modify[VMState](s => s.copy(out = s.out :+ res))
  _ <- advance
yield ()

def bdv(combo: Int): State[VMState, Unit] = for
  numerator <- State.get[VMState].map(_.regs.a)
  comboVal <- resolve(combo)
  denominator = Math.pow(2, comboVal).toInt
  res = numerator / denominator
  _ <- State.modify[VMState](s => s.copy(regs = s.regs.copy(b = res)))
  _ <- advance
yield ()

def cdv(combo: Int): State[VMState, Unit] = for
  numerator <- State.get[VMState].map(_.regs.a)
  comboVal <- resolve(combo)
  denominator = Math.pow(2, comboVal).toInt
  res = numerator / denominator
  _ <- State.modify[VMState](s => s.copy(regs = s.regs.copy(c = res)))
  _ <- advance
yield ()

def runVM: State[VMState, List[Int]] = for
  next <- State.inspect[VMState, Option[Zipper[Instruction]]](_.ip)
  state <- State.get[VMState]
  _ = println(state)
  result <- next match
    case Some(zipper) => execute(zipper.focus) >> runVM
    case None         => State.inspect[VMState, List[Int]](_.out)
yield result

def execute: Instruction => State[VMState, Unit] =
  case Adv(combo: Int) => adv(combo)
  case Bst(combo: Int) => bst(combo)
  case Bxl(lit: Int)   => bxl(lit)
  case Jnz(lit: Int)   => jnz(lit)
  case Bxc             => bxc
  case Out(combo: Int) => out(combo)
  case Bdv(combo: Int) => bdv(combo)
  case Cdv(combo: Int) => cdv(combo)

val Success(regs, is) = parser.parse(input)

val initState = VMState(regs, Some(Zipper.fromList(is)), List())

runVM.runA(initState).value.mkString(",")
