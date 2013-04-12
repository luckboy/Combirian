package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap

case class Environment(globalVarValues: IntMap[Value], localVarValues: Array[Value]) extends EnvironmentLike[Environment]
{
  override protected def createEnv(maxLocalVarCount: Int): Environment = {
    val newLocalVarValues = Array.fill(maxLocalVarCount)(ErrorValue("undefined local variable", Seq()): Value)
    Environment(globalVarValues, newLocalVarValues)
  }
}