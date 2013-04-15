package pl.luckboy.combirian.parser
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

case class Lexer() extends StdLexical
{
  delimiters ++= List(
      "\\", "->", "=", "(", ")", ";", "\n",
      "#",
      "+", "-", "*", "/", "%",
      "&", "|", "^", 
      "<<", ">>",
      "==", "!=", "<", "<=", ">", ">=")
  
  reserved ++= List(
	  "let", "in",
      "true", "false", "nil",
      "if", "else", "cond", "tuple", "array", "hash", "size", "haskey", "keys", "nth", "updated", "istypeof",
      "intfrom", "floatfrom")
  
  case class CharLit(chars: String) extends Token
  case class IntLit(chars: String) extends Token
  case class FloatLit(chars: String) extends Token
  
  override def token = identOrKeyword | intLit | floatLit | charLit | stringLit | newline | delim
  
  override def whitespaceChar = elem("space char", c => c <= ' ' && c != '\n' && c != EofCh)
  
  def identOrKeyword = (identChar ~ (( identChar | digit ) *)		^^ {
    case c ~ cs => 
      val s = (c :: cs).mkString("")
      if(reserved.contains(s)) Keyword(s) else Identifier(s)
  })
  
  def newline = elem('\n')											^^^ Keyword("\n")
  
  def octDigit = elem("", { c => c >= '0' && c <= '7' })
  def hexDigit = elem("", { c => c.isDigit || (c.toUpper >= 'A' && c.toUpper <= 'F') })
    
  def intLit = octIntLit | hexIntLit | decIntLit
  def octIntLit = (elem('0') ~ (octDigit +))						^^ { case c ~ cs => IntLit((c :: cs).mkString("")) }
  def hexIntLit = (elem('0') ~ (elem('x') | elem('X'))) ~ (hexDigit +) ^^ { case (c1 ~ c2) ~ cs3 => IntLit((List(c1, c2) ++ cs3).mkString("")) }
  def decIntLit = (digit +)											^^ { case cs => IntLit(cs.mkString("")) }
  
  def floatLit = (digit +) ~ '.' ~ (((digit +) ~ (exp ?)) ?) 				^^ {
    case cs1 ~ c2 ~ Some(cs3 ~ s4) => FloatLit((cs1 ++ List(c2) ++ cs3).mkString("") + s4)
    case cs1 ~ c2 ~ None           => FloatLit((cs1 ++ List(c2)).mkString(""))
  }
  
  def exp = (elem('E') | elem('e')) ~ ((elem('+') | elem('-')) ?) ~ (digit +) ^^ {
    case c1 ~ optC2 ~ cs3 => (List(c1) ++ optC2 ++ cs3).mkString("")
  }
  
  def escape = (
      elem('\\') ~> elem('b')												^^^ '\b'
      | elem('\\') ~> elem('t')												^^^ '\t'
      | elem('\\') ~> elem('n')												^^^ '\n'
      | elem('\\') ~> elem('f')												^^^ '\f'
      | elem('\\') ~> elem('r')												^^^ '\r'
      | elem('\\') ~> elem('"')												^^^ '"'
      | elem('\\') ~> elem('\'')											^^^ '\''
      | elem('\\') ~> elem('\\')											^^^ '\\'
      | elem('\\') ~> repN(3, octDigit)								 		^^ { cs => java.lang.Integer.parseInt(cs.mkString(""), 8).toChar }
      | elem('\\') ~> repN(2, octDigit)										^^ { cs => java.lang.Integer.parseInt(cs.mkString(""), 8).toChar }
      | elem('\\') ~> repN(1, octDigit)										^^ { cs => java.lang.Integer.parseInt(cs.mkString(""), 8).toChar })
      
  def charLit = '\'' ~> (escape | elem('\n') | chrExcept('\'', '\\', EofCh)) <~ '\'' ^^ { c => CharLit(c.toString) }
  def stringLit = '"' ~> ((escape | elem('\n') | chrExcept('"', '\\', EofCh)) *) <~ '"' ^^ { cs => StringLit(cs.mkString("")) }
}