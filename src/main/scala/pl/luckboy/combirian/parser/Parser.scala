package pl.luckboy.combirian.parser
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.Positional
import pl.luckboy.combirian.interp.LiteralValue
import pl.luckboy.combirian.interp.TrueValue
import pl.luckboy.combirian.interp.FalseValue
import pl.luckboy.combirian.interp.NilValue
import pl.luckboy.combirian.interp.CharValue
import pl.luckboy.combirian.interp.IntValue
import pl.luckboy.combirian.interp.FloatValue
import pl.luckboy.combirian.interp.StringValue
import pl.luckboy.combirian.interp.BuiltinFunValue
import pl.luckboy.combirian.interp.BuiltinFunction

object Parser extends StandardTokenParsers with PackratParsers
{
  override val lexical = Lexer()
  
  object NlMode extends Enumeration
  {
    val Nl, NoNl = Value
  }
  
  case class RighParser[T](parser: Parser[T]) {
    def ~~[U] (parser2: Parser[U])(implicit nlMode: NlMode.Value) =
      nlMode match {
      	case NlMode.Nl   => this ~- parser2
      	case NlMode.NoNl => parser ~ parser2
      }
    
    def ~~>[U] (parser2: Parser[U])(implicit nlMode: NlMode.Value) =
      nlMode match {
      	case NlMode.Nl   => this ~-> parser2
      	case NlMode.NoNl => parser ~> parser2
      }
    
    def <~~[U] (parser2: Parser[U])(implicit nlMode: NlMode.Value) =
      nlMode match {
      	case NlMode.Nl   => this <~- parser2
      	case NlMode.NoNl => parser <~ parser2
      }

    def ~+ (implicit nlMode: NlMode.Value) =
      nlMode match {
        case NlMode.Nl   => this -+
        case NlMode.NoNl => parser +
      }
    
    def ~* (implicit nlMode: NlMode.Value) =
      nlMode match {
        case NlMode.Nl   => this -*
        case NlMode.NoNl => parser *
      }
    
    def ~-[U] (parser2: Parser[U]) = parser ~ (rep("\n") ~> parser2)

    def ~->[U] (parser2: Parser[U]) = parser ~> (rep("\n") ~> parser2)

    def <~- [U] (parser2: Parser[U]) = parser <~ (rep("\n") ~> parser2)

    def -+ = parser ~ ((rep("\n") ~> parser) *) ^^ { case x ~ xs => x :: xs }
    
    def -* = ((this -+) ?) ^^ { _.getOrElse(Nil) }
  }
  
  implicit def parserToRighParser[T](parser: Parser[T]) = new RighParser(parser)
  implicit def elemToRighParser(elem: Elem) = new RighParser(elem)
  implicit def stringToRighParser(s: String) = RighParser[String](s)
  
  def p[T <: Positional](parser: Parser[T]) = positioned(parser)
  
  lazy val nl = rep1("\n")
  lazy val semi = (rep("\n") ~ ";" ~ rep("\n")) | nl

  lazy val bind = p((ident ~- ("=" ~-> noNlParsers.expr))			^^ { case s ~ t => Bind(s, t) })
  lazy val binds = bind ~ ((semi ~> bind) *)						^^ { case b ~ bs => b :: bs }
  
  lazy val arg = p(ident 											^^ Arg)
  
  lazy val trueVal = "true" ^^^ TrueValue
  lazy val falseVal = "false" ^^^ FalseValue
  lazy val nilVal = "nil" ^^^ NilValue
  lazy val charVal = elem("", _.isInstanceOf[lexical.CharLit])		^^ { e => CharValue(e.chars.head) }
  lazy val intVal = elem("", _.isInstanceOf[lexical.IntLit])		^^ {
    e => 
      if(e.chars.startsWith("0x") || e.chars.startsWith("0X"))
        IntValue(java.lang.Long.parseLong(e.chars.substring(2), 16))
      else if(e.chars.startsWith("0"))
        IntValue(java.lang.Long.parseLong(e.chars.substring(1), 8))
      else
        IntValue(java.lang.Long.parseLong(e.chars, 10))
  }
  lazy val floatVal = elem("", _.isInstanceOf[lexical.FloatLit])	^^ { e => FloatValue(e.chars.toDouble) }
  lazy val strVal = stringLit										^^ StringValue
  lazy val builtinFunVal1 = (
      "-" | "~" | "cond" | "tuple" | "array" | "hash" | "length" | "haskey" | "keys" | "nth" | "updated" | "istypeof" |
      "intfrom" | "floatfrom") ^^ {
    s => BuiltinFunValue(BuiltinFunction.withName(s))
  }
  lazy val builtinFunVal2 = ("#" ~> (
      "+" | "-" | "*" | "/" | "%" | 
      "&" | "|" | "^" | 
      "<<" | ">>" |
      "==" | "!=" | "<" | "<=" | ">" | ">=" |
      "::")) ^^ { 
    s => BuiltinFunValue(BuiltinFunction.withName("#" + s))
  }
  lazy val builtinFunVal = builtinFunVal1 | builtinFunVal2
  lazy val value = trueVal | falseVal | nilVal | charVal | intVal | floatVal | strVal | builtinFunVal 
  
  case class BinOp(s: String) extends Positional
  
  lazy val binOp1 = p("::"											^^ BinOp)
  lazy val binOp2 = p(("==" | "!=" | "<" | "<=" | ">" | ">=")		^^ BinOp)
  lazy val binOp3 = p(("<<" | ">>")									^^ BinOp)
  lazy val binOp4 = p(("&" | "|" | "^")								^^ BinOp)
  lazy val binOp5 = p(("+" | "-")									^^ BinOp)
  lazy val binOp6 = p(("*" | "/" | "%")								^^ BinOp)
 
  case class IfOp() extends Positional
  
  lazy val ifOp = p("if"											^^^ IfOp())
  
  case class Parsers()(implicit nlMode: NlMode.Value)
  {
    lazy val expr: PackratParser[Term] = expr1
    lazy val expr1: PackratParser[Term] = p(expr2 ~~ binOp1 ~- (expr1 | expr2) ^^ mkBinOp) | expr2
    lazy val expr2: PackratParser[Term] = p((expr2 | expr3) ~~ binOp2 ~- expr3 ^^ mkBinOp) | expr3
    lazy val expr3: PackratParser[Term] = p((expr3 | expr4) ~~ binOp3 ~- expr4 ^^ mkBinOp) | expr4
    lazy val expr4: PackratParser[Term] = p((expr4 | expr5) ~~ binOp4 ~- expr5 ^^ mkBinOp) | expr5
    lazy val expr5: PackratParser[Term] = p((expr5 | expr6) ~~ binOp5 ~- expr6 ^^ mkBinOp) | expr6
    lazy val expr6: PackratParser[Term] = p((expr6 | exprN) ~~ binOp6 ~- exprN ^^ mkBinOp) | exprN
    
    lazy val exprN: PackratParser[Term] = app | let | lambda | ifElse | simpleExpr
    lazy val simpleExpr = variable | literal | ("(" ~-> nlParsers.expr <~- ")")
  
    lazy val app = p(simpleExpr ~~ (simpleExpr ~+)					^^ { case t ~ ts => App(t, ts) })
    lazy val let = p("let" ~-> binds ~- ("in" ~-> expr)				^^ { case bs ~ t => Let(bs, t) })
    lazy val lambda = p("\\" ~> (arg +) ~ ("->" ~-> expr)			^^ { case as ~ t => Lambda(as, t) })
    lazy val ifElse = p(ifOp ~- simpleExpr ~- nlParsers.expr ~- ("else" ~-> expr) ^^ { 
      case io ~ t1 ~ t2 ~ t3 => 
        App(Literal(BuiltinFunValue(BuiltinFunction.withName("cond"))).setPos(io.pos), 
            List(Lambda(List(Arg("_true").setPos(t2.pos)), t2).setPos(t2.pos), Lambda(List(Arg("_false").setPos(t3.pos)), t3).setPos(t3.pos), t1))
    })
    lazy val variable = p(ident										^^ Var)
    lazy val literal = p(value										^^ Literal)
  }
  
  def mkBinOp(x: Term ~ BinOp ~ Term) =
    x match { 
      case t1 ~ (bo @ BinOp(s)) ~ t2 =>
        App(Literal(BuiltinFunValue(BuiltinFunction.withName("#" + s))).setPos(bo.pos), List(t1, t2))
    }
  
  val nlParsers = Parsers()(NlMode.Nl)  
  val noNlParsers = Parsers()(NlMode.NoNl)
  
  lazy val definition = p(ident ~ (arg *) ~ ("=" ~-> noNlParsers.expr)	^^ { case s ~ as ~ t => Def(s, as, t) })
  
  lazy val parseTree = p(((definition ~ ((semi ~> definition) *)) ?)			^^ {
      case Some(d ~ ds) => ParseTree(d :: ds)
      case None         => ParseTree(Nil)
    })
    
  def parse(s: String) = phrase(parseTree)(new lexical.Scanner(s))
}