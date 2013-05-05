package pl.luckboy.combirian.interp

object LazyEvaluator extends Evaluator[Environment]
{
  override def valueFromTerm(term: Term)(env: Environment) = {
    term match {
      case _: GlobalVar | _: TailRecGlobalVar | _: SharedLocalVar | _: NonSharedLocalVar =>
        eval(term)(env)
      case _ =>
        val newEnv = env.clone()
        LazyValue(eval(term)(newEnv).force)
    }
  }
}