package pl.luckboy.combirian.interp

trait Evaluator[Env <: EnvironmentLike[Env]]
{
  def eval(term: Term)(env: Env): Value =
    (term match {
      case App(fun, args, _)                                              =>
        val funValue = eval(fun)(env)
        val argValues = args.map { eval(_)(env) }
        argValues.find { _.isError } match {
          case None           => app(funValue, argValues)(env)
          case Some(errValue) => errValue
        }
      case Let(bindTerms, body, _)                                        =>
        val bindValues = bindTerms.map { eval(_)(env) }
        bindValues.find { _.isError } match {
          case None           => env.withLocalVars(bindValues)(eval(body))
          case Some(errValue) => errValue
        }
      case Lambda(closureVarIndexes, argCount, body, maxLocalVarCount, _) =>
        val closureVarValues = closureVarIndexes.map { env.localVarValue(_).shared }
        lambda(closureVarValues, argCount, body, maxLocalVarCount)(env)
      case GlobalVar(idx, _)                                              =>
        env.globalVarValue(idx)
      case TailRecGlobalVar(idx, _)                                       =>
        val value = env.globalVarValue(idx)
        if(!value.isError) TailRecFunValue(value) else value
      case SharedLocalVar(idx, _)                                         =>
        env.localVarValue(idx).shared
      case NonSharedLocalVar(idx, _)                                      =>
        env.localVarValue(idx).copyAsNonShared
      case Literal(value, _)                                              =>
        value
    }).withPos(term.pos)
  
  def app(funValue: Value, argValues: Seq[Value])(env: Env): Value
  
  def lambda(closureVarValues: Seq[Value], argCount: Int, body: Term, maxLocalVarCount: Int)(env: Env): Value
}