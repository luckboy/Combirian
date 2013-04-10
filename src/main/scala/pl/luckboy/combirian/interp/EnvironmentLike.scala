package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap

trait EnvironmentLike[+This <: EnvironmentLike[This]]
{
  val globalVarValues: IntMap[Value]
  val localVarValues: Vector[Value]
  
  def withLocalVars(values: Seq[Value]): This
  
  def varValue(idx: Int) = 
    if(idx >= 0)
      globalVarValues.getOrElse(idx, ErrorValue("undefined variable"))
    else
      if(localVarValues.size >= (-idx) - 1)
        localVarValues((-idx) - 1)
      else
        ErrorValue("undefined variable")
}