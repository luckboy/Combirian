package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap
import scala.util.parsing.input.Position

case class Tree(combinatorBinds: IntMap[CombinatorBind])
{
  def ++ (tree: Tree) = Tree(combinatorBinds ++ tree.combinatorBinds)
  
  def forFile(file: java.io.File) = Tree(IntMap() ++ combinatorBinds.mapValues { _.copy(file = Some(file)) })  
  
  override def toString = {
    val scope = Tree.StringScope(
        globalVarNames = IntMap() ++ combinatorBinds.map { case (idx, CombinatorBind(name, _, _)) => (idx, name) },
        localVarNames = IntMap(),
        localVarCount = 0)
    combinatorBinds.values.groupBy { _.file }.map {
      case (file, combBinds) =>
        "/* " + file.map { _.getPath }.getOrElse("--") + " */\n" +
        combBinds.map { _.toIntendedStringForScope(0, scope) }.mkString("\n")
    }.mkString("\n")
  }
}

object Tree
{
  case class StringScope(globalVarNames: IntMap[String], localVarNames: IntMap[String], localVarCount: Int)
  {
    def withLocalVarNames(names: Seq[String]) = 
      copy(
          localVarNames = localVarNames ++ names.zipWithIndex.map { case (name, i) => (i + localVarCount, name) }, 
          localVarCount = localVarCount + names.size)
          
    def withClosureVarNames(names: Seq[Option[String]]) =
      copy(
          localVarNames = localVarNames ++ names.zipWithIndex.flatMap { case (name, i) => name.map { (i + localVarCount, _) } }, 
          localVarCount = localVarCount + names.size)
  }
}

case class CombinatorBind(name: String, combinator: Combinator, file: Option[java.io.File])
{
  def toIntendedStringForScope(n: Int, scope: Tree.StringScope) =
    combinator.toIntendedStringForNameWithScope(n, name, scope)
}

case class Combinator(argNames: Seq[String], body: Term, localVarCount: Int)
{
  val argCount = argNames.size
  
  def toIntendedStringForNameWithScope(n: Int, name: String, scope: Tree.StringScope) = {
    val newScope = scope.withLocalVarNames(argNames)
    name + " " + argNames.mkString(" ") + " /* lvc=" + localVarCount + " */ = " + body.toIntendedStringForScope(n + 2, newScope)
  }
}

trait Term
{
  def pos: Position
  
  def toIntendedStringForScope(n: Int, scope: Tree.StringScope): String =
    this match {
      case App(fun, args, _) =>
        (Seq(fun) ++ args).map {
          term =>
            term match {
              case _: GlobalVar | _: TailRecGlobalVar | _: SharedLocalVar | _: NonSharedLocalVar => 
                term.toIntendedStringForScope(n + 2, scope)
              case _ =>
                "(" + term.toIntendedStringForScope(n + 2, scope) + ")"
            }
        }.mkString(" ")
      case Let(binds, body, _) =>
        val newScope = scope.withLocalVarNames(binds.map { _.name })
        "let " + 
        binds.zipWithIndex.map { case (bind, i) => bind.toIntendedStringForScope(n + 4, scope) + "/* l" + (i + scope.localVarCount) + " */" }.mkString("\n" + (" " * (n + 4))) +
        "\n" + (" " * n) + "in  " + body.toIntendedStringForScope(n + 4, newScope)
      case Lambda(closureVarIndexes, argNames, body, localVarCount, _) =>
        val tmpScope = scope.withLocalVarNames(closureVarIndexes.map { idx => scope.localVarNames.getOrElse(idx, "_l" + idx) })
        val newScope = tmpScope.withLocalVarNames(argNames)
        "/* " + closureVarIndexes.map { idx => scope.localVarNames.getOrElse(idx, "") + " (l" + idx + ")" }.mkString(", ") + " */" +
        "\\" + argNames.mkString(" ") + " /* lvc=" + localVarCount + " */ ->" + body.toIntendedStringForScope(n + 2, scope)
      case GlobalVar(idx, _) => 
        scope.globalVarNames.getOrElse(idx, "(_g" + idx + ")") + "/* g" + idx + " */"
      case TailRecGlobalVar(idx, _) =>
        scope.globalVarNames.getOrElse(idx, "(_g" + idx + ")") + "/* trg" + idx + " */"
      case SharedLocalVar(idx, _) =>
        scope.localVarNames.getOrElse(idx, "(_l" + idx + ")") + "/* sl" + idx + " */"
      case NonSharedLocalVar(idx, _) =>
        scope.localVarNames.getOrElse(idx, "(_l" + idx + ")") + "/* nsl" + idx + " */"
      case Literal(value, _) =>
        value.toString
    }
}

object Term
case class App(fun: Term, args: Seq[Term], pos: Position) extends Term
case class Let(binds: Seq[Bind], body: Term, pos: Position) extends Term
{
  val bindTerms = binds.map { _.body }
}
case class Lambda(closureVarIndexes: Seq[Int], argNames: Seq[String], body: Term, localVarCount: Int, pos: Position) extends Term
{
  val argCount = argNames.size
}
case class GlobalVar(idx: Int, pos: Position) extends Term
case class TailRecGlobalVar(idx: Int, pos: Position) extends Term
case class SharedLocalVar(idx: Int, pos: Position) extends Term
case class NonSharedLocalVar(idx: Int, pos: Position) extends Term
case class Literal(value: LiteralValue, pos: Position) extends Term

case class Bind(name: String, body: Term)
{
  def toIntendedStringForScope(n: Int, scope: Tree.StringScope): String =
    name + " = " + body.toIntendedStringForScope(n + 2, scope)
}