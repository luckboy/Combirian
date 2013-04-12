package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap
import scala.util.parsing.input.Position

case class Tree(combinatorBinds: IntMap[CombinatorBind])

case class CombinatorBind(name: String, combinator: Combinator, file: Option[java.io.File])

case class Combinator(argCount: Int, body: Term, localVarCount: Int)

trait Term
{
  def pos: Position
}
case class App(fun: Term, args: Seq[Term], pos: Position) extends Term
case class Let(bindTerms: Seq[Term], body: Term, pos: Position) extends Term
case class Lambda(closureVarIndexes: Seq[Int], argCount: Int, body: Term, localVarCount: Int, pos: Position) extends Term
case class GlobalVar(idx: Int, pos: Position) extends Term
case class SharedLocalVar(idx: Int, pos: Position) extends Term
case class NonSharedLocalVar(idx: Int, pos: Position) extends Term
case class Literal(value: LiteralValue, pos: Position) extends Term