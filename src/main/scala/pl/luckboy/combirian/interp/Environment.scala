package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap

class Environment(values1: IntMap[Value], values2: Array[Value], val currentFile: Option[java.io.File]) extends EnvironmentLike[Environment]
{
  override protected val globalVarValues = values1
  override protected val localVarValues = values2
  
  override protected def createEnv(maxLocalVarCount: Int): Environment = {
    val newLocalVarValues = Array.fill(maxLocalVarCount)(ErrorValue("undefined local variable", Seq()): Value)
    new Environment(globalVarValues, newLocalVarValues, currentFile)
  }
  
  override def withGlobalVars(values: Map[Int, Value]) = new Environment(globalVarValues ++ values, localVarValues, currentFile)
  
  override def withCurrentFile(file: Option[java.io.File]) = new Environment(globalVarValues, localVarValues, file)
}

object Environment extends EnvironmentFactory[Environment]
{
  override def empty = Environment(IntMap(), Seq(), None)
  
  def apply(globalVarValues: IntMap[Value], localVarValues: Seq[Value], currentFile: Option[java.io.File]) =
    new Environment(globalVarValues, localVarValues.toArray, currentFile)
}