package pl.luckboy.combirian.interp

trait Evaluator[Env <: EnvironmentLike[Env]]
{
  def eval(term: Term)(env: Env): Value =
    (term match {
      case App(fun, args, _)         =>
        val funValue = eval(fun)(env)
        valuesFromTerms(args)(env) match {
          case Right(argValues) => funValue(argValues)(this)(env)
          case Left(errValue)   => errValue
        }
      case letTerm @ Let(_, body, _) =>
        valuesFromTerms(letTerm.bindTerms)(env) match {
          case Right(bindValues) => env.withLocalVars(bindValues)(eval(body))
          case Left(errValue)    => errValue
        }
      case lambda: Lambda            =>
        val closureVarValues = lambda.closureVarIndexes.map { env.localVarValue(_).copyAsShared }
        NonSharedLambdaValue(closureVarValues.map { _.copyAsShared }, lambda)
      case GlobalVar(idx, _)         =>
        env.globalVarValue(idx)
      case TailRecGlobalVar(idx, _)  =>
        val value = env.globalVarValue(idx)
        if(!value.isError) TailRecFunValue(value) else value
      case SharedLocalVar(idx, _)    =>
        env.localVarValue(idx).copyAsShared
      case NonSharedLocalVar(idx, _) =>
        env.localVarValue(idx).copyAsNonShared
      case Literal(value, _)         =>
        value
    }).withPos(term.pos)
  
  def valuesFromTerms(terms: Seq[Term])(env: Env) = {
    val values = new Array[Value](terms.size)
    var errValue = None: Option[Value]
    var i = 0
    val termIter = terms.iterator 
    while(i < values.length && errValue.isEmpty) {
      val value = valueFromTerm(termIter.next())(env)
      if(value.isError) errValue = Some(value) else values(i) = value
      i += 1
    }
    errValue.toLeft(values.toSeq)
  }
  
  def valueFromTerm(term: Term)(env: Env) = eval(term)(env)

  def postFullApply(funValue: Value, argValues: Seq[Value])(retValue: Value)(env: Env) = retValue
}