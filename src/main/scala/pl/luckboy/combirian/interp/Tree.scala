/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap
import scala.util.parsing.input.Position

case class Tree(combinatorBinds: IntMap[CombinatorBind])
{
  def ++ (tree: Tree) = Tree(combinatorBinds ++ tree.combinatorBinds)
  
  def forFile(file: java.io.File) = Tree(IntMap() ++ combinatorBinds.mapValues { _.copy(file = Some(file)) })  
  
  override def toString = toString(true)
  
  def toString(canShowNames: Boolean) = {
    val scope = Tree.StringScope(
        globalVarNames = IntMap() ++ combinatorBinds.map { case (idx, CombinatorBind(name, _, _)) => (idx, name) },
        localVarNames = IntMap(),
        localVarCount = 0)
    combinatorBinds.groupBy { _._2.file }.map {
      case (file, combBinds) =>
        "// " + file.map { _.getPath }.getOrElse("--") + "\n\n" +
        combBinds.map { 
          case (idx, combBind) =>
            combBind.toIntendedStringForScope(0, scope, canShowNames) + " //<g" + idx + ">" }.mkString("\n\n")
        }.mkString("\n")
  }
}

object Tree
{
  val empty = Tree(IntMap())
  
  case class StringScope(globalVarNames: IntMap[String], localVarNames: IntMap[String], localVarCount: Int)
  {
    def withLocalVarNames(names: Seq[String]) = 
      copy(
          localVarNames = localVarNames ++ names.zipWithIndex.map { case (name, i) => (i + localVarCount, name) }, 
          localVarCount = localVarCount + names.size)          
          
    def withLocalVars(n: Int) = copy(localVarCount = localVarCount + n)
  }
}

case class CombinatorBind(name: String, combinator: Combinator, file: Option[java.io.File])
{
  def toIntendedStringForScope(n: Int, scope: Tree.StringScope, canShowNames: Boolean) =
    combinator.toIntendedStringForNameWithScope(n, name, scope, canShowNames)
}

case class Combinator(argNames: Seq[String], body: Term, localVarCount: Int)
{
  val argCount = argNames.size
  
  def toIntendedStringForNameWithScope(n: Int, name: String, scope: Tree.StringScope, canShowNames: Boolean) = {
    val newScope = if(canShowNames) scope.withLocalVarNames(argNames) else scope.withLocalVars(argNames.size)
    name + " " + argNames.map { _ + " " }.mkString("") + 
    (if(localVarCount != 0) "/*lvc=" + localVarCount + "*/ = " else "= ") +
    body.toIntendedStringForScope(n + 2, newScope, canShowNames) 
  }
}

trait Term
{
  def pos: Position
  
  private def globalVarName(idx: Int, scope: Tree.StringScope, canShowNames: Boolean) =
    if(canShowNames)
      scope.globalVarNames.getOrElse(idx, "<g" + idx + ">")
    else
      "<g" + idx + ">"
  
  private def localVarName(idx: Int, scope: Tree.StringScope, canShowNames: Boolean) =
    if(canShowNames)
      scope.localVarNames.getOrElse(idx, "<l" + idx + ">")
    else
      "<l" + idx + ">"
  
  def toIntendedStringForScope(n: Int, scope: Tree.StringScope, canShowNames: Boolean): String =
    this match {
      case App(fun, args, _) =>
        (Seq(fun) ++ args).map {
          term =>
            term match {
              case _: GlobalVar | _: TailRecGlobalVar | _: SharedLocalVar | _: NonSharedLocalVar | _: Literal => 
                term.toIntendedStringForScope(n + 2, scope, canShowNames)
              case _ =>
                "(" + term.toIntendedStringForScope(n + 2, scope, canShowNames) + ")"
            }
        }.mkString(" ")
      case Let(binds, body, _) =>
        val newScope = scope.withLocalVarNames(binds.map { _.name })
        "let\n" + (" " * (n + 2)) +
        binds.zipWithIndex.map { 
          case (bind, i) => 
            bind.toIntendedStringForScope(n + 2, scope, canShowNames) + " //<l" + (i + scope.localVarCount) + ">"
        }.mkString("\n" + (" " * (n + 2))) +
        "\n" + (" " * n) + "in\n" + (" " * (n + 2)) + body.toIntendedStringForScope(n + 2, newScope, canShowNames)
      case Lambda(closureVarIndexes, argNames, body, localVarCount, _, _) =>
        val tmpScope1 = scope.copy(localVarNames = IntMap(), localVarCount = 0)
        val tmpScope2 = tmpScope1.withLocalVarNames(closureVarIndexes.map { idx => scope.localVarNames.getOrElse(idx, "l" + idx) })
        val newScope = tmpScope2.withLocalVarNames(argNames)
        ({
          if(!closureVarIndexes.isEmpty)
            "/*" + closureVarIndexes.zipWithIndex.map { 
              case (idx, newIdx) => localVarName(newIdx, scope, canShowNames) + "=<l" + idx  + ">"
            }.mkString(",") + "*/ "
          else
            ""
        }) + 
        "\\" + argNames.map { _ + " " }.mkString("") + 
        (if(localVarCount != 0) "/*lvc=" + localVarCount + "*/ -> "  else "-> ") + 
        body.toIntendedStringForScope(n + 2, newScope, canShowNames)
      case GlobalVar(idx, _) => 
        globalVarName(idx, scope, canShowNames)
      case TailRecGlobalVar(idx, _) =>
        globalVarName(idx, scope, canShowNames) + "/*t*/"
      case SharedLocalVar(idx, _) =>
        localVarName(idx, scope, canShowNames)
      case NonSharedLocalVar(idx, _) =>
        localVarName(idx, scope, canShowNames) + "/*n*/"
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
case class Lambda(closureVarIndexes: Seq[Int], argNames: Seq[String], body: Term, localVarCount: Int, combinatorIdx: Int, pos: Position) extends Term
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
  def toIntendedStringForScope(n: Int, scope: Tree.StringScope, canShowNames: Boolean): String =
    name + " = " + body.toIntendedStringForScope(n + 2, scope, canShowNames: Boolean)
}
