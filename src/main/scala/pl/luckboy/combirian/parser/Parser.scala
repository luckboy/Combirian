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

    def -+ = parser ~ ((nl ~> parser) *) ^^ { case x ~ xs => x :: xs }
    
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
        IntValue(java.lang.Integer.parseInt(e.chars.substring(2), 16))
      else if(e.chars.startsWith("0"))
        IntValue(java.lang.Integer.parseInt(e.chars.substring(1), 8))
      else
        IntValue(java.lang.Integer.parseInt(e.chars, 10))
  }
  lazy val floatVal = elem("", _.isInstanceOf[lexical.FloatLit])	^^ { e => FloatValue(e.chars.toFloat) }
  lazy val strVal = stringLit										^^ StringValue
  lazy val builtinFunVal = (
      "neg" | "+" | "-" | "*" | "/" | "%" | 
      "not" | "&" | "|" | "^" | 
      "<<" | ">>" |
      "==" | "!=" | "<" | "<=" | ">" | ">=" |
      "::" | 
      "array" | "arraylength" | "nth" | "update" |
      "istypeof") ^^ { case s => BuiltinFunValue(BuiltinFunction.withName(s)) }
  lazy val value = trueVal | falseVal | nilVal | charVal | intVal | floatVal | strVal | builtinFunVal 
  
  case class Parsers()(implicit nlMode: NlMode.Value)
  {
    lazy val expr: PackratParser[Term] = app | let | lambda | simpleExpr
    lazy val simpleExpr = variable | literal | ("(" ~-> nlParsers.expr <~- ")")
  
    lazy val app = p(simpleExpr ~~ (simpleExpr ~+)					^^ { case t ~ ts => App(t, ts) })
    lazy val let = p("let" ~-> binds ~- ("in" ~-> expr)				^^ { case bs ~ t => Let(bs, t) })
    lazy val lambda = p("\\" ~> (arg +) ~ ("." ~-> expr)			^^ { case as ~ t => Lambda(as, t) })
    lazy val variable = p(ident										^^ Var)
    lazy val literal = p(value										^^ Literal)
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