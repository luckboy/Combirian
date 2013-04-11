package pl.luckboy.combirian.parser
import scala.util.parsing.input.Positional
import pl.luckboy.combirian.interp.LiteralValue

case class ParseTree(defs: List[Def]) extends Positional
{
  override def toString = defs.mkString("\n")
}

case class Def(name: String, args: Seq[Arg], body: Term) extends Positional
{
  override def toString = name + " " + args.mkString(" ") + " = " + body.toIntendedString(2)
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
        "let " ++ binds.map { _.toIntendedString(n + 4) }.mkString("\n" + (" " * (n + 4))) + "\n" + (" " * n) + "in  " + body.toIntendedString(n + 4)
      case Lambda(args, body) =>
        "\\" + args.mkString(" ") + " -> " + body.toIntendedString(n + 2)
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