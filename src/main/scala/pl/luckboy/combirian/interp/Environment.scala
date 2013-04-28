package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap

class Environment(values1: IntMap[Value], values2: Array[Value]) extends EnvironmentLike[Environment]
{
  override protected val globalVarValues = values1
  override protected val localVarValues = values2
  
  override protected def createEnv(maxLocalVarCount: Int): Environment = {
    val newLocalVarValues = Array.fill(maxLocalVarCount)(ErrorValue("undefined local variable", Seq()): Value)
    new Environment(globalVarValues, newLocalVarValues)
  }
  
  override def withGlobalVars(values: Map[Int, Value]) = new Environment(globalVarValues ++ values, localVarValues)
}

object Environment extends EnvironmentFactory[Environment]
{
  override def empty = Environment(IntMap(), Seq())
  
  def apply(globalVarValues: IntMap[Value], localVarValues: Seq[Value]) =
    new Environment(globalVarValues, localVarValues.toArray)
}