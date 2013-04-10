package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap

case class Tree(combinators: IntMap[Combinator])

case class Combinator(argCount: Int, body: Term, maxLocalVarCount: Int)

trait Term
case class App(fun: Term, args: Seq[Term]) extends Term
case class Let(bindTerms: Seq[Term], body: Term) extends Term
case class Lambda(closureVarIndexes: Seq[Int], argCount: Int, body: Term, maxLocalVarCount: Int) extends Term
case class Var(idx: Int) extends Term
case class Literal(value: LiteralValue) extends Term

