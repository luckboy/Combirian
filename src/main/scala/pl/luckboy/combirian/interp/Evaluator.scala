package pl.luckboy.combirian.interp

trait Evaluator[Env <: EnvironmentLike[Env]]
{
  def eval(term: Term)(env: Env): Value =
    term match {
      case App(fun, args)                                              =>
        val funValue = eval(fun)(env)
        val argValues = args.map { eval(_)(env) }
        app(funValue, argValues)(env)
      case Let(bindTerms, body)                                        =>
        val bindValues = bindTerms.map { eval(_)(env) }
        eval(body)(env.withLocalVars(bindValues))
      case Lambda(closureVarIndexes, argCount, body, maxLocalVarCount) =>
        val closureVarValues = closureVarIndexes.map(env.varValue)
        lambda(closureVarValues, argCount, body, maxLocalVarCount)(env)
      case Var(idx)                                                    =>
        env.varValue(idx)
      case Literal(value)                                              =>
        value
    }
  
  def app(funValue: Value, argValues: Seq[Value])(env: Env): Value
  
  def lambda(closureVarValues: Seq[Value], argCount: Int, body: Term, maxLocalVarCount: Int)(env: Env): Value
}