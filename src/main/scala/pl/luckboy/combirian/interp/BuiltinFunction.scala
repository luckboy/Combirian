package pl.luckboy.combirian.interp

object BuiltinFunction extends Enumeration 
{
  val Neg = Value("neg")
  val Add = Value("+")
  val Sub = Value("-")
  val Mul = Value("*")
  val Div = Value("/")
  val Mod = Value("%")
  val Not = Value("not") 
  val And = Value("&")
  val Or = Value("|")
  val Xor = Value("^")
  val LeftShift = Value("<<")
  val RightShift = Value(">>")
  val Eq = Value("==")
  val NotEq = Value("!=")
  val Lt = Value("<")
  val Le = Value("<=")
  val Gt = Value(">")
  val Ge = Value(">=")
  val Cons = Value("::")
  val Array = Value("array")
  val Arraylength = Value("arraylength")
  val Nth = Value("nth")
  val Update = Value("update")
  val Istypeof = Value("istypeof")
}