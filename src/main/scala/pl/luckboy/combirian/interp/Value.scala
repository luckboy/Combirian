package pl.luckboy.combirian.interp
import scala.collection.mutable.HashMap
import scala.util.parsing.input.Position

trait Value
{  
  def isError = false
  
  def withPos(pos: Position): Value = this
  
  def withFileAndName(file: Option[java.io.File], name: Option[String]): Value = this
  
  def shared: Value = this
  
  def copyAsNonShared: Value = this
}

case class ListValue(elems: List[Value]) extends Value
case class PartialAppValue(args: Seq[Value], fun: FunValue) extends Value
case class ErrorValue(message: String, stackTraceParts: Seq[ErrorStackTracePart]) extends Value
{
  override def isError = true
    
  private def lastStackTracePartAndOther =
    stackTraceParts.lastOption.map { (_, stackTraceParts.init) }.getOrElse((ErrorStackTracePart(None, None, Seq()), Seq()))
  
  override def withPos(pos: Position) = {
    val (lastPart, otherParts) = lastStackTracePartAndOther
    ErrorValue(message, otherParts :+ lastPart.copy(poses = lastPart.poses :+ pos))
  }
    
  override def withFileAndName(file: Option[java.io.File], name: Option[String]) = {
    val (lastPart, otherParts) = lastStackTracePartAndOther
    ErrorValue(message, (otherParts :+ lastPart.copy(file = file, name = name)) :+ ErrorStackTracePart(None, None, Seq()))
  }
  
  def stackTrace: Seq[ErrorStackTraceElement] =
    stackTraceParts.flatMap { part => part.poses.map { pos => ErrorStackTraceElement(part.file, part.name, pos) } }
}

trait ArrayValue extends Value
{
  def elems: Seq[Value]
}
case class SharedArrayValue(elems: Seq[Value]) extends ArrayValue
{
  override def copyAsNonShared = NonSharedArrayValue(elems.toArray.clone)
}
case class NonSharedArrayValue(array: Array[Value]) extends ArrayValue
{
  def elems: Seq[Value] = array.toSeq
  
  override def shared = SharedArrayValue(elems)
}

trait HashValue extends Value
{
  def elems: Map[Value, Value]
}
case class SharedHashValue(elems: Map[Value, Value]) extends HashValue
{
  override def copyAsNonShared = NonSharedHashValue(HashMap[Value, Value]() ++= elems)
}
case class NonSharedHashValue(hashMap: HashMap[Value, Value]) extends HashValue
{
  def elems = hashMap.toMap

  override def shared = SharedHashValue(elems)
}

trait FunValue extends Value
case class CombinatorValue(combinatorBind: CombinatorBind) extends FunValue
case class LambdaValue(closure: Seq[Value], lambda: Lambda) extends FunValue

trait LiteralValue extends Value
{
  override def toString =
    this match {
      case TrueValue            => "true"
      case FalseValue           => "false"
      case NilValue             => "nil"
      case CharValue(x)         => "'" + (if(x == '\'') "\\'" else x.toString) + "'"
      case IntValue(x)          => x.toString
      case FloatValue(x)        => x.toString
      case StringValue(x)       => "\"" + x.toList.map { c => if(c == '"') "\\\"" else c.toString }.mkString("") + "\""
      case BuiltinFunValue(fun) => fun.toString
    }
}

case object TrueValue extends LiteralValue
case object FalseValue extends LiteralValue
case object NilValue extends LiteralValue
case class CharValue(x: Char) extends LiteralValue
case class IntValue(x: Long) extends LiteralValue
case class FloatValue(x: Double) extends LiteralValue
case class StringValue(x: String) extends LiteralValue
case class BuiltinFunValue(fun: BuiltinFunction.Value) extends LiteralValue