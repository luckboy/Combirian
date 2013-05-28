/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian.interp
import scala.collection.mutable
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scala.annotation.tailrec

// Value

trait Value
{  
  def isError = false
  
  def withPos(pos: Position): Value = this
  
  def withFileAndName(file: Option[java.io.File], name: Option[String]): Value = this
  
  def shared: SharedValue
  
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
        case SharedPartialAppValue(fun, args)    => NonSharedPartialAppValue(fun, args ++ argValues)
        case NonSharedPartialAppValue(fun, args) => NonSharedPartialAppValue(fun, args ++ argValues)
        case _                                   => NonSharedPartialAppValue(funValue, argValues)
      }
  
  def apply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env) = 
    recApply(this, argValues)(eval)(env)
  
  def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env): Value = 
    ErrorValue("non-applicative value", Seq())

  def force: EvaluatedValue
}

trait SharedValue extends Value
{
  override def shared: SharedValue = this
}

trait EvaluatedValue extends Value
{
  override def force: EvaluatedValue = this
  
  override def shared: SharedValue
}

trait UnevaluatedValue extends Value
{
  override def argCount = force.argCount
  
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env) =
    force.fullApply(argValues)(eval)(env)      

  override def toString = force.toString
}

case class TailRecFunValue(fun: Value) extends SharedValue with EvaluatedValue
{
  override def argCount = fun.argCount
  
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env) =
    if(argCount == argValues.size)
      TailRecAppValue(fun, argValues)
    else
      ErrorValue("incorrect number of arguments", Seq())
  
  override def toString = "<tailrecfun>"
}

case class TailRecAppValue(fun: Value, args: Seq[Value]) extends SharedValue with EvaluatedValue
{
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env): Value =
    ErrorValue("value of tail recursive application", Seq())
    
  override def toString = "<tailrecapp>"
}

class LazyValue(f: => EvaluatedValue) extends SharedValue with UnevaluatedValue
{
  override lazy val force = f
  
  override def copyAsNonShared = NonSharedLazyValue(this)
  
  override def shared = SharedLazyValue(this)
}

object LazyValue
{
  def apply(f: => EvaluatedValue) = new LazyValue(f)
  
  def unapply(value: LazyValue) = Some(value.force)
}

case class SharedLazyValue(value: UnevaluatedValue) extends SharedValue with UnevaluatedValue
{
  override def copyAsNonShared = NonSharedLazyValue(this)
  
  override def force = value.force.shared.force
}

case class NonSharedLazyValue(value: UnevaluatedValue) extends Value with UnevaluatedValue
{
  override def shared = SharedLazyValue(this)
  
  override def force = value.force.copyAsNonShared.force  
}

case class ErrorValue(message: String, stackTrace: Seq[ErrorStackTraceElement]) extends SharedValue with EvaluatedValue
{
  def stackTraceString = "error: " + message + "\n" + stackTrace.filter { _.isFinal }.mkString("\n")

  override def isError = true
    
  private def lastStackTraceElemAndOthers =
    stackTrace.lastOption.filterNot { _.isFinal }.map { (_, stackTrace.init) }.getOrElse((ErrorStackTraceElement(None, None, NoPosition, false), Seq()))
  
  override def withPos(pos: Position) = {
    val (last, others) = lastStackTraceElemAndOthers
    if(last.pos == NoPosition) ErrorValue(message, others :+ last.copy(pos = pos)) else this
  }
    
  override def withFileAndName(file: Option[java.io.File], name: Option[String]) = {
    val (last, others) = lastStackTraceElemAndOthers
    ErrorValue(message, (others :+ last.copy(file = file, name = name, isFinal = true)) :+ ErrorStackTraceElement(None, None, NoPosition, false))
  }
  
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env) = this
  
  override def toString = "<error: " + message + ">"  
}

// AbstractSeqValue

trait AbstractSeqValue extends EvaluatedValue
{
  def elems: Seq[Value]

  def updated(i: Long, value: Value): Value
}

object AbstractSeqValue
{
  def unapply(value: AbstractSeqValue) = Some(value.elems)
}

// TupleValue

trait TupleValue extends AbstractSeqValue
{
  override def toString = "(" + elems.mkString(", ") + ")"  
}

object TupleValue
{
  def unapply(value: TupleValue) = Some(value.elems)
}

case class SharedTupleValue(elems: Seq[SharedValue]) extends TupleValue with SharedValue
{
  override def copyAsNonShared = NonSharedTupleValue(elems.map { _.copyAsNonShared })
  
  override def updated(i: Long, value: Value) =
    if(i >= 0 && i < elems.size)
      SharedTupleValue(elems.updated(i.toInt, value.shared))
    else
      ErrorValue("index out of bounds", Seq())
}

case class NonSharedTupleValue(elems: Seq[Value]) extends TupleValue
{
  override def shared = SharedTupleValue(elems.map { _.shared })

  override def updated(i: Long, value: Value) = 
    if(i >= 0 && i < elems.size)
      NonSharedTupleValue(elems.updated(i.toInt, value))
    else
      ErrorValue("index out of bounds", Seq())
}

// VectorValue

case class VectorValue(elems: Seq[SharedValue]) extends AbstractSeqValue with SharedValue
{
  override def updated(i: Long, value: Value) =
    if(i >= 0 && i < elems.size)
      VectorValue(elems.updated(i.toInt, value.shared))
    else
      ErrorValue("index out of bounds", Seq())

  override def toString = "[" + elems.mkString(", ") + "]"
}

// ArrayValue

trait ArrayValue extends AbstractSeqValue
{
  override def elems: Seq[SharedValue]

  override def toString = "#[" + elems.mkString(", ") + "]"
}

object ArrayValue
{
  def unapply(value: ArrayValue) = Some(value.elems)
}

case class SharedArrayValue(elems: Seq[SharedValue]) extends ArrayValue with SharedValue
{
  override def copyAsNonShared = NonSharedArrayValue(elems.toArray.clone)

  override def updated(i: Long, value: Value) =
    if(i >= 0 && i < elems.size)
      NonSharedArrayValue(elems.updated(i.toInt, value.shared).toArray)
    else
      ErrorValue("index out of bounds", Seq())
}

case class NonSharedArrayValue(array: Array[SharedValue]) extends ArrayValue
{
  override def elems = array.toSeq
  
  override def shared = SharedArrayValue(elems)

  override def updated(i: Long, value: Value) =
    if(i >= 0 && i < array.length) {
      array(i.toInt) = value.shared
      this
    } else
      ErrorValue("index out of bounds", Seq())
}

// AbstractMapValue

trait AbstractMapValue extends Value with EvaluatedValue
{
  def elems: Map[SharedValue, SharedValue]
  
  def updated(key: Value, value: Value): Value
  
  def removed(key: Value): Value
}

object AbstractMapValue
{
  def unapply(value: AbstractMapValue) = Some(value.elems)
}

// MapValue

case class MapValue(elems: Map[SharedValue, SharedValue]) extends AbstractMapValue with SharedValue
{
  override def updated(key: Value, value: Value) = MapValue(elems.updated(key.shared, value.shared))
  
  override def removed(key: Value) = MapValue(elems - key.shared)

  override def toString = "{" + elems.map { case (key, value) => "(" + key + ", " + value + ")" }.mkString(", ") + "}"
}

// HashValue

trait HashValue extends AbstractMapValue
{
  override def elems: Map[SharedValue, SharedValue]

  override def toString = "#{" + elems.map { case (key, value) => "(" + key + ", " + value + ")" }.mkString(", ") + "}"
}

object HashValue
{
  def unapply(value: HashValue) = Some(value.elems)
}

case class SharedHashValue(elems: Map[SharedValue, SharedValue]) extends HashValue with SharedValue
{
  override def copyAsNonShared = NonSharedHashValue(mutable.HashMap[SharedValue, SharedValue]() ++= elems)

  override def updated(key: Value, value: Value) = NonSharedHashValue(mutable.HashMap[SharedValue, SharedValue]() ++= elems.updated(key.shared, value.shared))

  override def removed(key: Value) = NonSharedHashValue(mutable.HashMap[SharedValue, SharedValue]() ++= (elems - key.shared))
}

case class NonSharedHashValue(hashMap: mutable.HashMap[SharedValue, SharedValue]) extends HashValue
{
  override def elems = hashMap.toMap

  override def shared = SharedHashValue(elems)

  override def updated(key: Value, value: Value) = {
    hashMap(key.shared) = value.shared
    this
  }
  
  override def removed(key: Value) = {
    hashMap -= key.shared
    this
  }
}

// FunValue

trait FunValue extends EvaluatedValue

case class CombinatorValue(idx: Int, combinatorBind: CombinatorBind) extends FunValue with SharedValue
{
  override def argCount = combinatorBind.combinator.argCount
  
  @tailrec
  override final def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env): Value =
    if(argCount == argValues.size) {
      val body = combinatorBind.combinator.body
      val localVarCount = combinatorBind.combinator.localVarCount
      env.withClosureAndArgs(Seq(), argValues, localVarCount)(eval.eval(body)) match {
        case retValue @ TailRecAppValue(fun, args) =>
          if(this eq fun) fullApply(args)(eval)(env) else retValue
        case retValue                              =>
          retValue.withFileAndName(combinatorBind.file, Some(combinatorBind.name))
      }
    } else 
      ErrorValue("incorrect number of arguments", Seq())
      
  override def toString = combinatorBind.name
}

trait LambdaValue extends FunValue
{
  def closure: Seq[Value]

  def lambda: Lambda

  override def argCount = lambda.argCount
  
  @tailrec
  override final def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env): Value =
    if(argCount == argValues.size)
      env.withClosureAndArgs(closure, argValues, lambda.localVarCount)(eval.eval(lambda.body)) match {
        case retValue @ TailRecAppValue(fun, args) =>
          if(this eq fun) fullApply(args)(eval)(env) else retValue
        case retValue                              =>
          if(!retValue.isError)
            retValue
          else
            env.globalVarValue(lambda.combinatorIdx) match {
              case CombinatorValue(_, combBind) => retValue.withFileAndName(combBind.file, None)
              case _                            => retValue.withFileAndName(env.currentFile, None)
            }
      }
    else
      ErrorValue("incorrect number of arguments", Seq())
      
  override def toString = "<lambda>"
}

case class SharedLambdaValue(closure: Seq[SharedValue], lambda: Lambda) extends LambdaValue with SharedValue

case class NonSharedLambdaValue(closure: Seq[Value], lambda: Lambda) extends LambdaValue
{
  def shared = SharedLambdaValue(closure.map { _.shared }, lambda)
}

// Partial application

trait PartialAppValue extends EvaluatedValue
{
  def fun: Value
  
  def args: Seq[Value]

  override def argCount = fun.argCount - args.size
  
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env) =
    if(argCount == argValues.size)
      fun.fullApply(args ++ argValues)(eval)(env)
    else
      ErrorValue("incorrect number of arguments", Seq())
  
  override def toString =
    (Seq(fun) ++ args).map {
      value =>
        value match {
          case _: PartialAppValue => "(" + value + ")"
          case _                  => value.toString
        }
    }.mkString(" ")
}

case class SharedPartialAppValue(fun: SharedValue, args: Seq[SharedValue]) extends PartialAppValue with SharedValue

case class NonSharedPartialAppValue(fun: Value, args: Seq[Value]) extends PartialAppValue
{
  override def shared = SharedPartialAppValue(fun.shared, args.map { _.shared })
}

// LiteralValue

trait LiteralValue extends SharedValue with EvaluatedValue
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
      case TupleFunValue(n)     => "tuple " + n 
    }
}

trait BooleanValue extends LiteralValue
{
  def isTrue = this == TrueValue

  def isFalse = this == FalseValue
}

object BooleanValue
{
  def unapply(value: BooleanValue) = Some(value.isTrue)
}

case object TrueValue extends BooleanValue
case object FalseValue extends BooleanValue
case object NilValue extends LiteralValue
case class CharValue(x: Char) extends LiteralValue
case class IntValue(x: Long) extends LiteralValue
case class FloatValue(x: Double) extends LiteralValue
case class StringValue(x: String) extends LiteralValue

trait LiteralFunValue extends LiteralValue with FunValue

trait BuiltinFunValue extends LiteralFunValue
{
  def fun: BuiltinFunction.Value
}

object BuiltinFunValue
{
  def apply(fun: BuiltinFunction.Value) = BuiltinFunValues.builtinFunValues(fun)
  
  def unapply(value: BuiltinFunValue) = Some(value.fun)
}

case class TupleFunValue(n: Int) extends LiteralFunValue
{
  override def argCount = n
  
  override def fullApply[Env <: EnvironmentLike[Env]](argValues: Seq[Value])(eval: Evaluator[Env])(env: Env): Value = 
    if(argCount == argValues.size)
      NonSharedTupleValue(argValues)
    else
      ErrorValue("incorrect number of arguments", Seq())      
}
