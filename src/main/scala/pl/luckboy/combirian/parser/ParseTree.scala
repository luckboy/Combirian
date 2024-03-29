/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian.parser
import scala.util.parsing.input.Positional
import pl.luckboy.combirian.interp.LiteralValue

case class ParseTree(defs: List[Def]) extends Positional
{
  override def toString = defs.mkString("\n\n")
}

case class Def(name: String, args: Seq[Arg], body: Term) extends Positional
{
  override def toString = name + " " + args.map { _ + " " }.mkString("") + "= " + body.toIntendedString(2)
}

trait Term extends Positional
{
  def toIntendedString(n: Int): String =
    this match {
      case App(fun, args) =>
        (Seq(fun) ++ args).map {
          term =>
            term match {
              case _: Var | _: Literal => term.toIntendedString(n + 2)
              case _                   => "(" + term.toIntendedString(n + 2) + ")"
            }
        }.mkString(" ")
      case Let(binds, body) =>
        "let\n" + (" " * (n + 2)) + binds.map { _.toIntendedString(n + 4) }.mkString("\n" + (" " * (n + 2))) + "\n" +
        (" " * n) + "in\n" + (" " * (n + 2)) + body.toIntendedString(n + 4)
      case Lambda(args, body) =>
        "\\" + args.map { _ + " " }.mkString("") + "-> " + body.toIntendedString(n + 2)
      case Var(name) =>
        name
      case Literal(value) =>
        value.toString
    }
  
  override def toString = toIntendedString(0)
}
case class App(fun: Term, args: Seq[Term]) extends Term
case class Let(binds: List[Bind], body: Term) extends Term
case class Lambda(args: List[Arg], body: Term) extends Term
case class Var(name: String) extends Term
case class Literal(value: LiteralValue) extends Term

case class Bind(name: String, body: Term) extends Positional
{
  def toIntendedString(n: Int) = name + " = " + body.toIntendedString(n + 2)

  override def toString = toIntendedString(0)
}

case class Arg(name: String) extends Positional
{
  override def toString = name
}
