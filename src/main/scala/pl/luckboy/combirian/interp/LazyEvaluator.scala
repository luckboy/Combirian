package pl.luckboy.combirian.interp

object LazyEvaluator extends Evaluator[Environment]
{
  override def valueFromTerm(term: Term)(env: Environment) = LazyValue(eval(term)(env.clone))
}