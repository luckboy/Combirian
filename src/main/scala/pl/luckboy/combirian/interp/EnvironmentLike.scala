/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap

@cloneable
trait EnvironmentLike[+This <: EnvironmentLike[This]]
{
  def repr: This = asInstanceOf[This]
  
  protected val globalVarValues: IntMap[Value]
  protected val localVarValues: Array[Value]
  protected var localDefinedVarCount = 0
  
  protected def createEnv(maxLocalVarCount: Int): This
  
  def withClosureAndArgs(closureVarValues: Seq[Value], argValues: Seq[Value], localVarCount: Int)(f: This => Value) =
    createEnv(closureVarValues.size + argValues.size + localVarCount).withLocalVars(closureVarValues ++ argValues)(f)
  
  def withLocalVars(values: Seq[Value])(f: This => Value) =
    if(localDefinedVarCount + values.size <= localVarValues.length) {
      var i = 0
      while(i < values.size) {
        localVarValues(localDefinedVarCount + i) = values(i)
        i += 1
      }
      localDefinedVarCount += values.size
      val res = f(repr)
      localDefinedVarCount -= values.size
      res
    } else
      ErrorValue("stack overflow", Seq())

  def globalVarValue(idx: Int) = globalVarValues.getOrElse(idx, ErrorValue("undefined global variable", Seq()))

  def localVarValue(idx: Int) = if(localDefinedVarCount >= idx) localVarValues(idx) else ErrorValue("undefined local variable", Seq())
  
  def withGlobalVars(values: Map[Int, Value]): This
  
  def globalVarIdxs = globalVarValues.keySet
  
  def currentFile: Option[java.io.File]
  
  def withCurrentFile(file: Option[java.io.File]): This
  
  override def clone: This = {
    val newEnv = createEnv(localVarValues.length)
    localVarValues.copyToArray(newEnv.localVarValues)
    newEnv.localDefinedVarCount = localDefinedVarCount
    newEnv
  }
}
