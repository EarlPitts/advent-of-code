import util._

val List(rulesStr, printStr) =
  unsafeGetInputRaw.split("\n\n").map(_.strip).toList

type Rules = Map[Int, List[Int]]

val rules = rulesStr
  .split("\n")
  .map(_.split("\\|"))
  .map(_.map(_.toInt))

val pages = printStr
  .split("\n")
  .toList
  .map(_.split(",").toList)
  .map(_.map(_.toInt))

val ruleMap = rules.foldLeft(Map.empty[Int, List[Int]]) { (m, rule) =>
  val List(before, after) = rule.toList
  m.updatedWith(before) {
    case None         => Some(List(after))
    case Some(afters) => Some(after :: afters)
  }
}

// ------------- Part 1 --------------

def isRight(rules: Rules, pages: List[Int]): Boolean =
  def go(ps: List[Int]): Boolean = ps match
    case curr :: rest =>
      rest.forall { i =>
        !rules(i).contains(curr)
      } && go(rest)
    case Nil => true
  go(pages)

def middle(pages: List[Int]): Int =
  pages.drop((pages.size - 1) / 2).head

pages
  .filter { print => isRight(ruleMap, print.toList) }
  .map(middle)
  .sum

// ------------- Part 2 --------------

def insert(rules: Rules, elem: Int, l: List[Int]): List[Int] =
  l match
    case Nil => List(elem)
    case e :: es =>
      if rules(elem).contains(e) then elem :: e :: es
      else e :: insert(rules, elem, es)

def fixOrder(rules: Rules)(pages: List[Int]): List[Int] =
  def go(ps: List[Int], fixed: List[Int]): List[Int] = ps match
    case curr :: rest => go(rest, insert(rules, curr, fixed))
    case Nil          => fixed
  go(pages, List.empty[Int])

pages
  .filterNot { print => isRight(ruleMap, print.toList) }
  .map(fixOrder(ruleMap))
  .map(middle)
  .sum
