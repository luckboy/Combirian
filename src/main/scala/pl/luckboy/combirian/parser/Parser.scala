package pl.luckboy.combirian.parser
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.Positional
import scala.util.parsing.input.NoPosition
import scala.io.Source
import pl.luckboy.combirian.interp.LiteralValue
import pl.luckboy.combirian.interp.TrueValue
import pl.luckboy.combirian.interp.FalseValue
import pl.luckboy.combirian.interp.NilValue
import pl.luckboy.combirian.interp.CharValue
import pl.luckboy.combirian.interp.IntValue
import pl.luckboy.combirian.interp.FloatValue
import pl.luckboy.combirian.interp.StringValue
import pl.luckboy.combirian.interp.BuiltinFunValue
import pl.luckboy.combirian.interp.TupleFunValue
import pl.luckboy.combirian.interp.BuiltinFunction
import pl.luckboy.combirian.ResultUtils._

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

  lazy val integer = elem("", _.isInstanceOf[lexical.IntLit])		^^ {
    e => 
      if(e.chars.startsWith("0x") || e.chars.startsWith("0X"))
        Integer.parseInt(e.chars.substring(2), 16)
      else if(e.chars.startsWith("0"))
        Integer.parseInt(e.chars.substring(1), 8)
      else
        Integer.parseInt(e.chars, 10)
  }
  
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
      "-" | "~" | "cond" | "uncurry" | "vector" | "array" | "size" | "haskey" | "keys" | "nth" | "updated" | 
      "istypeof" | 
      "charfrom" | "intfrom" | "floatfrom" | "stringfrom" | "tuplefrom" | "vectorfrom" | "mapfrom" | "arrayfrom" | "hashfrom") ^^ {
    s => BuiltinFunValue(BuiltinFunction.withName(s))
  }
  lazy val builtinFunVal2 = ("#" ~> (
      "+" | "-" | "*" | "/" | "%" | 
      "&" | "|" | "^" | 
      "<<" | ">>" |
      "==" | "!=" | "<" | "<=" | ">" | ">=")) ^^ { 
    s => BuiltinFunValue(BuiltinFunction.withName("#" + s))
  }
  lazy val builtinFunVal3 = (
      "tuple" ~-> integer											^^ TupleFunValue
      )
  lazy val builtinFunVal = builtinFunVal1 | builtinFunVal2 | builtinFunVal3
  lazy val value = trueVal | falseVal | nilVal | charVal | intVal | floatVal | strVal | builtinFunVal 
  
  case class BinOp(s: String) extends Positional
  
  lazy val binOp1 = p(("==" | "!=" | "<" | "<=" | ">" | ">=")		^^ BinOp)
  lazy val binOp2 = p(("<<" | ">>")									^^ BinOp)
  lazy val binOp3 = p(("&" | "|" | "^")								^^ BinOp)
  lazy val binOp4 = p(("+" | "-")									^^ BinOp)
  lazy val binOp5 = p(("*" | "/" | "%")								^^ BinOp)
 
  case class Positioned() extends Positional
  
  lazy val ifOp = p("if"											^^^ Positioned())

  lazy val leftParen = p("("										^^^ Positioned())
  lazy val leftBracket = p("["										^^^ Positioned())
  lazy val leftBrace = p("{"										^^^ Positioned())
  lazy val hashLeftBracket = p("#" ~ "["							^^^ Positioned())
  lazy val hashLeftBrace = p("#" ~ "{"								^^^ Positioned())
  
  case class Parsers()(implicit nlMode: NlMode.Value)
  {
    lazy val expr: PackratParser[Term] = expr1
    lazy val expr1: PackratParser[Term] = p((expr1 | expr2) ~~ binOp1 ~- expr2 ^^ mkBinOp) | expr2
    lazy val expr2: PackratParser[Term] = p((expr2 | expr3) ~~ binOp2 ~- expr3 ^^ mkBinOp) | expr3
    lazy val expr3: PackratParser[Term] = p((expr3 | expr4) ~~ binOp3 ~- expr4 ^^ mkBinOp) | expr4
    lazy val expr4: PackratParser[Term] = p((expr4 | expr5) ~~ binOp4 ~- expr5 ^^ mkBinOp) | expr5
    lazy val expr5: PackratParser[Term] = p((expr5 | exprN) ~~ binOp5 ~- exprN ^^ mkBinOp) | exprN
    
    lazy val exprN: PackratParser[Term] = app | let | lambda | ifElse | simpleExpr
    lazy val simpleExpr = variable | literal | unit | tuple | array | hash | ("(" ~-> nlParsers.expr <~- ")")
    
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

    lazy val unit = "(" ~- ")"										^^^ Literal(TupleFunValue(0))
    lazy val tuple = leftParen ~- nlParsers.expr ~- (("," ~-> nlParsers.expr) -+) <~- ")" ^^ {
      case lp ~ t ~ ts => App(Literal(TupleFunValue(ts.size + 1)).setPos(lp.pos), t :: ts)
    }
    lazy val vector = leftBracket ~- ((nlParsers.expr ~- (("," ~-> nlParsers.expr) -*)) ?) <~- "]" ^^ mkColl(BuiltinFunction.Vectorfrom)
    lazy val map = leftBrace ~- ((nlParsers.pair ~- (("," ~-> nlParsers.pair) -*)) ?) <~- "}" ^^ mkColl(BuiltinFunction.Mapfrom)
    lazy val array = hashLeftBracket ~- ((nlParsers.expr ~- (("," ~-> nlParsers.expr) -*)) ?) <~- "]" ^^ mkColl(BuiltinFunction.Arrayfrom)
    lazy val hash = hashLeftBrace ~- ((nlParsers.pair ~- (("," ~-> nlParsers.pair) -*)) ?) <~- "}" ^^ mkColl(BuiltinFunction.Hashfrom)
    lazy val pair = leftParen ~- nlParsers.expr ~- ("," ~-> nlParsers.expr) <~- ")" ^^ {
      case lp ~ t1 ~ t2 => App(Literal(TupleFunValue(2)).setPos(lp.pos), List(t1, t2))
    }
  }
  
  def mkBinOp(x: Term ~ BinOp ~ Term) =
    x match { 
      case t1 ~ (bo @ BinOp(s)) ~ t2 =>
        App(Literal(BuiltinFunValue(BuiltinFunction.withName("#" + s))).setPos(bo.pos), List(t1, t2))
    }
  
  def mkColl(fun: BuiltinFunction.Value)(x: Positioned ~ Option[Term ~ List[Term]]) =
    x match {
      case p ~ optTs => 
        val ts = optTs.map { case t2 ~ ts2 => t2 :: ts2 }.getOrElse(Nil)
        App(Literal(BuiltinFunValue(fun)).setPos(p.pos),
            List(App(Literal(TupleFunValue(ts.size)).setPos(p.pos), ts).setPos(p.pos)))
    }
  
  val nlParsers = Parsers()(NlMode.Nl)  
  val noNlParsers = Parsers()(NlMode.NoNl)
  
  lazy val definition = p(ident ~ (arg *) ~ ("=" ~-> noNlParsers.expr)	^^ { case s ~ as ~ t => Def(s, as, t) })
  
  lazy val parseTree = p(((definition ~ ((semi ~> definition) *)) ?)			^^ {
      case Some(d ~ ds) => ParseTree(d :: ds)
      case None         => ParseTree(Nil)
    })
    
  def parse(s: String): Either[Seq[ParserError], ParseTree] = 
    phrase(parseTree)(new lexical.Scanner(s)) match {
      case Success(res, _)    => Right(res)
      case Failure(msg, next) => Left(Seq(ParserError(None, next.pos, msg)))
      case Error(msg, next)   => Left(Seq(ParserError(None, next.pos, "fatal: " + msg)))
    }
  
  def parse(in: java.io.InputStream): Either[Seq[ParserError], ParseTree] = {
    try {
      val src = Source.fromInputStream(in).withClose { case () => in.close() }
      parse(src.mkString)
    } catch {
      case e: java.io.IOException =>
        Left(Seq(ParserError(None, NoPosition, "io error: " + e.getMessage())))
    }
  }.left.map { errs => errs.sortWith { (err1, err2) => err1.pos < err2.pos } }
  
  def parse(file: java.io.File): Either[Seq[ParserError], ParseTree] = {
    val res = try {
      parse(new java.io.FileInputStream(file))
    } catch {
      case e: java.io.IOException =>
        Left(Seq(ParserError(None, NoPosition, "io error: " + e.getMessage())))
    }
    resultForFile(res, file)
  }

  def parse(files: Set[java.io.File]): Either[Seq[ParserError], Map[java.io.File, ParseTree]] =
    files.foldLeft(Right(Map()): Either[Seq[ParserError], Map[java.io.File, ParseTree]]) {
      case (Right(parseTrees), file) => parse(file).right.map { parseTree => parseTrees + (file -> parseTree) }
      case (Left(errs), _)           => Left(errs)
    }
}