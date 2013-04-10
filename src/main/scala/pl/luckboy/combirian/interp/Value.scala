package pl.luckboy.combirian.interp

trait Value

case class ListValue(xs: List[Value]) extends Value
case class ArrayValue(xs: Array[Value]) extends Value
case class FunValue(closureValues: Seq[Value], argCount: Int, body: Term) extends Value
case class PartialAppValue(args: Seq[Value], fun: FunValue) extends Value
case class ErrorValue(message: String) extends Value

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
case class IntValue(x: Int) extends LiteralValue
case class FloatValue(x: Float) extends LiteralValue
case class StringValue(x: String) extends LiteralValue
case class BuiltinFunValue(fun: BuiltinFunction.Value) extends LiteralValue