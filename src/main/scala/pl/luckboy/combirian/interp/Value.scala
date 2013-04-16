package pl.luckboy.combirian.interp
import scala.collection.mutable.HashMap
import scala.util.parsing.input.Position
import scala.annotation.tailrec

// Value

trait Value
{  
  def isError = false
  
  def withPos(pos: Position): Value = this
  
  def withFileAndName(file: Option[java.io.File], name: Option[String]): Value = this
  
  def shared: Value = this
  
  def copyAsNonShared: Value = this

  def argCount = 1
  
  @tailrec
  private def recApply[Env <: EnvironmentLike[Env]](funValue: Value, argValues: Seq[Value])(eval: Evaluator[Env])(env: Env): Value = 
    if(argCount == argValues.size) {
      eval.postFullApply(funValue, argValues)(funValue.fullApply(argValues)(eval)(env))(env)
    } else if(argCount < argValues.size) {
      val (passedArgValues, otherArgValues) = argValues.splitAt(funValue.argCount)
      val retValue = eval.postFullApply(funValue, passedArgValues)(funValue.fullApply(passedArgValues)(eval)(env))(env)
      if(!retValue.isError) recApply(retValue, otherArgValues)(eval)(env) else retValue
    } else
      funValue match {
        case PartialAppValue(fun, args) => PartialAppValue(fun, args ++ argValues.map { _.shared })
        case _                          => PartialAppValue(funValue, argValues.map { _.shared })
      }
  
  def apply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env) = 
    recApply(this, argValues)(eval)(env)
  
  def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env): Value = 
    ErrorValue("inapplicable value", Seq())
    
  def force: Value = this
}

case class PartialAppValue(fun: Value, args: Seq[Value]) extends Value
{
  override def argCount = fun.argCount - args.size
  
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env) =
    if(argCount == argValues.size)
      fun.fullApply(args ++ argValues)(eval)(env)
    else
      ErrorValue("incorrect number of arguments", Seq())
}

case class TailRecFunValue(fun: Value) extends Value
{
  override def argCount = fun.argCount
  
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env) =
    if(argCount == argValues.size)
      TailRecAppValue(fun, argValues)
    else
      ErrorValue("incorrect number of arguments", Seq())
}

case class TailRecAppValue(fun: Value, args: Seq[Value]) extends Value
{
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env): Value =
    ErrorValue("value of tail recursive application", Seq())
}

class LazyValue(f: => Value) extends Value
{
  override def argCount = force.argCount
  
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env) =
    force.fullApply(argValues)(eval)(env)
      
  override lazy val force = f
}

object LazyValue
{
  def apply(f: => Value) = new LazyValue(f)
  
  def unapply(value: LazyValue) = Some(value.force)
}

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
  
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env) = this
}

// ArrayValue

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

// HashValue

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

// FunValue

trait FunValue extends Value

case class CombinatorValue(idx: Int, combinatorBind: CombinatorBind) extends FunValue
{
  override def argCount = combinatorBind.combinator.argCount
  
  @tailrec
  override final def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env): Value =
    if(argCount == argValues.size) {
      val body = combinatorBind.combinator.body
      val localVarCount = combinatorBind.combinator.localVarCount
      env.withClosureAndArgs(Seq(), argValues, localVarCount)(eval.eval(body)) match {
        case TailRecAppValue(fun, args) =>
          if(this eq fun) fullApply(args)(eval)(env) else ErrorValue("tail recursion error: this eq fun == false", Seq())
        case retValue                   =>
          retValue
      }
    } else 
      ErrorValue("incorrect number of arguments", Seq())
}

case class LambdaValue(closure: Seq[Value], lambda: Lambda) extends FunValue
{
  override def argCount = lambda.argCount
  
  @tailrec
  override final def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env): Value =
    if(argCount == argValues.size)
      env.withClosureAndArgs(Seq(), argValues, lambda.localVarCount)(eval.eval(lambda.body)) match {
        case TailRecAppValue(fun, args) =>
          if(this eq fun) fullApply(args)(eval)(env) else ErrorValue("tail recursion error: this eq fun == false", Seq())
        case retValue                   => 
          retValue
      }
    else     
      ErrorValue("incorrect number of arguments", Seq())
}

// LiteralValue

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