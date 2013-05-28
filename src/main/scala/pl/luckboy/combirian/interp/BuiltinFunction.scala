/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian.interp

object BuiltinFunction extends Enumeration 
{
  val Neg = Value("neg")
  val Add = Value("#+")
  val Sub = Value("#-")
  val Mul = Value("#*")
  val Div = Value("#/")
  val Mod = Value("#%")
  val Not = Value("not") 
  val And = Value("#&")
  val Or = Value("#|")
  val Xor = Value("#^")
  val LeftShift = Value("#<<")
  val RightShift = Value("#>>")
  val Eq = Value("#==")
  val NotEq = Value("#!=")
  val Lt = Value("#<")
  val Le = Value("#<=")
  val Gt = Value("#>")
  val Ge = Value("#>=")
  val Cond = Value("cond")
  val Uncurry = Value("uncurry")
  val Vector = Value("vector")
  val Array = Value("array")
  val Size = Value("size")
  val Haskey = Value("haskey")
  val Keys = Value("keys")
  val Keyarray = Value("keyarray")
  val Nth = Value("nth")
  val Updated = Value("updated")
  val Istypeof = Value("istypeof")
  val Charfrom = Value("charfrom")
  val Intfrom = Value("intfrom")
  val Floatfrom = Value("floatfrom")
  val Stringfrom = Value("stringfrom")
  val Tuplefrom = Value("tuplefrom")
  val Vectorfrom = Value("vectorfrom")
  val Mapfrom = Value("hashfrom")
  val Arrayfrom = Value("arrayfrom")
  val Hashfrom = Value("hashfrom")
  val Size2 = Value("size2")
  val Haskey2 = Value("haskey2")
  val Keys2 = Value("keys2")
  val Keyarray2 = Value("keyarray2")
  val Nth2 = Value("nth2")
}
