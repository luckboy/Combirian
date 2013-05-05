package pl.luckboy.combirian.interp

object LazyEvaluator extends Evaluator[Environment]
{
  override def valueFromTerm(term: Term)(env: Environment) = {
    term match {
      case _: App =>
        val newEnv = env.clone()
        LazyValue(eval(term)(newEnv).force)
      case _      =>
        eval(term)(env)
    }
  }
}