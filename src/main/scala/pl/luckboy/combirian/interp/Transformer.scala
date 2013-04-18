package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap
import scala.util.parsing.input.Position
import pl.luckboy.combirian.parser

object Transformer
{
  case class Scope(
      globalVarIdxs: Map[String, Int],
      localVarIdxs: Map[String, Int], 
      localVarRefCounts: Map[String, Int],
      currentCombinatorInfo: Option[TailRecInfo])
  {
    def withLocalVarIdxs(idxs: Iterable[(String, Int)]) = copy(localVarIdxs = localVarIdxs ++ idxs)

    def withGlobalVarIdxs(idxs: Iterable[(String, Int)]) = copy(globalVarIdxs = globalVarIdxs ++ idxs)
  }
  
  case class TailRecInfo(name: String, argCount: Int)
  
  private def zipResults[T, U](res1: Either[Seq[TransformerError], T], res2: Either[Seq[TransformerError], U]) =
    (res1, res2) match {
      case (Right(x), Right(y))       => Right(x, y)
      case (Left(errs1), Left(errs2)) => Left(errs1 ++ errs2)
      case (Left(errs), _)            => Left(errs)
      case (_, Left(errs))            => Left(errs)
    }

  private def resultForFile[T](res: Either[Seq[TransformerError], T], file: java.io.File) =
    res match {
      case Right(x)   => Right(x)
      case Left(errs) => Left(errs.map { _.copy(file = Some(file)) })
    }
  
  private def closureVarIndexesFromTerm(term: parser.Term)(localVarIdxs: Map[String, Int]): Map[String, Int] =
    term match {
      case parser.App(fun, args)     =>
        closureVarIndexesFromTerm(fun)(localVarIdxs) ++ args.flatMap { closureVarIndexesFromTerm(_)(localVarIdxs) }
      case parser.Let(binds, body)   =>
        closureVarIndexesFromTerm(body)(localVarIdxs -- binds.map { _.name })
      case parser.Lambda(args, body) =>
        closureVarIndexesFromTerm(body)(localVarIdxs -- args.map { _.name })
      case parser.Var(name)          =>
        Map() ++ localVarIdxs.get(name).map { (name, _) } 
      case parser.Literal(_)         =>
        Map()
    }
  
  private def localVarCountFromTerm(term: parser.Term): Int =
    term match {
      case parser.App(fun, args)   => localVarCountFromTerm(fun) + args.foldLeft(0) { _ + localVarCountFromTerm(_) }
      case parser.Let(binds, body) => binds.size + localVarCountFromTerm(body)
      case _                       => 0
    }
  
  private def localVarRefCountsFromTerm(term: parser.Term)(localVarNames: Set[String])(localVarRefCounts: Map[String, Int]): Map[String, Int] =
    term match {
      case parser.App(fun @ parser.Literal(BuiltinFunValue(BuiltinFunction.Cond)), Seq(firstTerm: parser.Lambda, secondTerm: parser.Lambda, condTerm)) =>
        val zeros = localVarRefCounts.keySet.map { (_, 0) }.toMap
        localVarRefCountsFromTerm(condTerm)(localVarNames)(
            sumLocalVarRefCounts(localVarRefCounts, 
                maxLocalVarRefCounts(
                    localVarRefCountsFromTerm(firstTerm)(localVarNames)(zeros),
                    localVarRefCountsFromTerm(secondTerm)(localVarNames)(zeros)
                    )))
      case parser.App(fun, args)     => 
        args.foldLeft(localVarRefCountsFromTerm(fun)(localVarNames)(localVarRefCounts)) {
          (localVarRefCounts2, arg) => localVarRefCountsFromTerm(arg)(localVarNames)(localVarRefCounts2)
        }
      case parser.Let(binds, body)   =>
        val newLocalVarRefCounts = binds.foldLeft(localVarRefCounts) { 
          (counts, bind) => localVarRefCountsFromTerm(bind.body)(localVarNames)(counts) 
        }
        localVarRefCountsFromTerm(body)(localVarNames -- binds.map { _.name })(newLocalVarRefCounts)
      case parser.Lambda(args, body) =>
        localVarRefCountsFromTerm(body)(localVarNames -- args.map { _.name })(localVarRefCounts)
      case parser.Var(name)          =>
        if(localVarNames.contains(name))
          localVarRefCounts.get(name).map { count => localVarRefCounts + (name -> (count + 1)) }.getOrElse(localVarRefCounts)
        else
          localVarRefCounts
      case _                         =>
        localVarRefCounts
  }
  
  private def maxLocalVarRefCounts(counts1: Map[String, Int], counts2:  Map[String, Int]): Map[String, Int] =
    counts1.map { case (name, count1) => (name, count1.max(counts2.getOrElse(name, 0))) }

  private def sumLocalVarRefCounts(counts1: Map[String, Int], counts2:  Map[String, Int]): Map[String, Int] =
    counts1.map { case (name, count1) => (name, count1 + counts2.getOrElse(name, 0)) }
  
  private def diffLocalVarRefCounts(counts1: Map[String, Int], counts2:  Map[String, Int]): Map[String, Int] =
    counts1.map { case (name, count1) => (name, count1 - counts2.getOrElse(name, 0)) }
  
  def transformTerms(terms: Seq[parser.Term])(scope: Scope): Either[Seq[TransformerError], Seq[Term]] =
    terms.foldLeft(Right(Nil): Either[Seq[TransformerError], Seq[Term]]) {
      (res, term) => zipResults(res, transformTerm(term, false)(scope)).right.map { case (terms2, term2) => terms2 :+ term2 }
    }
  
  private def app(fun: parser.Term, args: Seq[parser.Term], pos: Position)(scope: Scope) =
    zipResults(transformTerm(fun, false)(scope), transformTerms(args)(scope)).right.map {
      case (fun2, args2) => App(fun2, args2, pos)
    }
  
  private def lambda(args: Seq[parser.Arg], body: parser.Term, pos: Position, combInfo: Option[TailRecInfo])(scope: Scope) = {
    val closureVarIndexes = closureVarIndexesFromTerm(body)(scope.localVarIdxs)
    val tmpLocalVarRefCounts1 = scope.localVarRefCounts -- (scope.localVarRefCounts.keySet -- closureVarIndexes.keySet)
    val tmpLocalVarRefCounts2 = localVarRefCountsFromTerm(body)(args.map { _.name }.toSet)(args.map { arg => (arg.name, 0) }.toMap)    
    val newLocalVarRefCounts = sumLocalVarRefCounts(tmpLocalVarRefCounts1, tmpLocalVarRefCounts2)
    val tmpScope = scope.copy(
        localVarIdxs = closureVarIndexes, 
        localVarRefCounts = newLocalVarRefCounts,
        currentCombinatorInfo = combInfo)
    val argIdxs = args.zipWithIndex.map { case (arg, i) => (arg.name, i + tmpScope.localVarIdxs.size) }
    val newScope = tmpScope.withLocalVarIdxs(argIdxs)
    transformTerm(body, combInfo.isDefined)(newScope).right.map { 
      Lambda(closureVarIndexes.values.toSeq, args.map { _.name }, _, localVarCountFromTerm(body), pos) 
    }
  }
  
  def transformTerm(term: parser.Term, canTailRec: Boolean)(scope: Scope): Either[Seq[TransformerError], Term] =
    term match {
      case parser.App(fun @ parser.Var(funName), args) if canTailRec =>
        scope.currentCombinatorInfo.map {
          combInfo =>
            if(combInfo.name == funName && !scope.localVarIdxs.contains(funName) && combInfo.argCount == args.size)
              transformTerms(args)(scope).right.flatMap {
                args2 =>
                  scope.globalVarIdxs.get(combInfo.name).map {
                    idx => Right(App(TailRecGlobalVar(idx, fun.pos), args2, term.pos))
                  }.getOrElse(Left(Seq(TransformerError(None, term.pos, "undefined variable " + funName))))                  
              }
            else
              app(fun, args, term.pos)(scope)
        }.getOrElse(app(fun, args, term.pos)(scope))
      case parser.App(fun @ parser.Literal(BuiltinFunValue(BuiltinFunction.Cond)), Seq(firstTerm, secondTerm, condTerm)) =>
        zipResults(
            transformTerm(firstTerm, canTailRec)(scope), 
            zipResults(
                transformTerm(secondTerm, canTailRec)(scope), 
                transformTerm(condTerm, false)(scope))).right.map {
          case (firstTerm2, (secondTerm2, condTerm2)) =>
            App(Literal(BuiltinFunValue(BuiltinFunction.Cond), fun.pos), Seq(firstTerm2, secondTerm2, condTerm2), term.pos)
        }
      case parser.App(fun, args)     =>
        app(fun, args, term.pos)(scope)
      case parser.Let(binds, body)   =>
        val newLocalVarIdxs = binds.zipWithIndex.map { case (bind, i) => (bind.name, i + scope.localVarIdxs.size) }
        val tmpScope = scope.withLocalVarIdxs(newLocalVarIdxs)
        val newLocalVarNames = newLocalVarIdxs.map { _._1 }.toSet
        val zeros = newLocalVarNames.map { (_, 0) }.toMap
        val newLocalVarRefCounts = localVarRefCountsFromTerm(body)(newLocalVarNames)(zeros)
        val newScope = tmpScope.copy(localVarRefCounts = tmpScope.localVarRefCounts ++ newLocalVarRefCounts)
        zipResults(transformTerms(binds.map { _.body })(scope), transformTerm(body, canTailRec)(newScope)).right.map {
          case (bindTerms2, body2) => Let(binds.map { _.name }.zip(bindTerms2).map(Bind.tupled), body2, term.pos)
        }
      case parser.Lambda(args, body) =>
        lambda(args, body, term.pos, None)(scope)
      case parser.Var(name)          =>
        scope.localVarIdxs.get(name).map {
          idx =>
            scope.localVarRefCounts.get(name).map { 
              count => if(count == 1) NonSharedLocalVar(idx, term.pos) else SharedLocalVar(idx, term.pos)
            }.getOrElse(SharedLocalVar(idx, term.pos))
        }.orElse {
          scope.globalVarIdxs.get(name).map { GlobalVar(_, term.pos) }
        }.toRight(Seq(TransformerError(None, term.pos, "undefined variable " + name)))
      case parser.Literal(value)     =>
        Right(Literal(value, term.pos))
    }
  
  def scopeFromParseTree(parseTree: parser.ParseTree)(scope: Scope): Either[Seq[TransformerError], Scope] = {
    val globalVarIdxsWithPoses = parseTree.defs.zipWithIndex.map {
      case (definition @ parser.Def(name, args, body), idx) => (name, (idx, definition.pos))
    }.toMap
    val alreadyDefinedCombNamesWithPoses = (scope.globalVarIdxs.keySet & globalVarIdxsWithPoses.keySet).flatMap {
      name => globalVarIdxsWithPoses.get(name).map { case (_, pos) => (name, pos) }.toSet
    }.toSeq
    if(alreadyDefinedCombNamesWithPoses.isEmpty)
      Right(scope.withGlobalVarIdxs(globalVarIdxsWithPoses.mapValues { _._1 } ))
    else
      Left(alreadyDefinedCombNamesWithPoses.map { 
        case (name, pos) => TransformerError(None, pos, "already defined global variable " + name) 
      })
  }.left.map { errs => errs.sortWith { (err1, err2) => err1.pos < err2.pos } }
  
  def transformParseTree(parseTree: parser.ParseTree)(scope: Scope): Either[Seq[TransformerError], Tree] =
    parseTree.defs.foldLeft(Right(IntMap()): Either[Seq[TransformerError], IntMap[CombinatorBind]]) {
      case (res, definition @ parser.Def(name, args, body)) =>
        val argIdxs = args.zipWithIndex.map { case (arg, idx) => (arg.name, idx) }.toMap
        val combInfo = TailRecInfo(name, args.size)
        val newScope = scope.copy(localVarIdxs = argIdxs, currentCombinatorInfo = Some(combInfo))
        val res2 = body match {
          case parser.Lambda(lambdaArgs, lambdaBody) if args.isEmpty =>
            lambda(lambdaArgs, lambdaBody, body.pos, Some(combInfo))(newScope)
          case _                                                     =>
            transformTerm(body, true)(newScope)
        }
        zipResults(res, res2).right.map {
          case (combBinds, body2)=> {
            val idx = scope.globalVarIdxs(name)
            (combBinds + (idx -> CombinatorBind(name, Combinator(args.map { _.name }, body2, localVarCountFromTerm(body)), None)))
          }
        }
    }.right.map(Tree.apply).left.map { errs => errs.sortWith { (err1, err2) => err1.pos < err2.pos } }
    
  def transform(parseTrees: Map[java.io.File, parser.ParseTree], cmdParseTree: Option[parser.ParseTree])(tree: Tree): Either[Seq[TransformerError], Tree] = {
    val allParseTrees = parseTrees.map { case (file, parseTree) => (Some(file), parseTree) } ++ cmdParseTree.map { (None, _) }
    allParseTrees.foldLeft(Right(Scope(Map(), Map(), Map(), None)): Either[Seq[TransformerError], Scope]) {
      case (Right(scope), (file, parseTree)) => scopeFromParseTree(parseTree)(scope)
      case (Left(errs), _)                   => Left(errs)
    }.right.flatMap {
      scope =>
        allParseTrees.foldLeft((Right(tree), tree): (Either[Seq[TransformerError], Tree], Tree)) {
          case ((res, newTree), (file, parseTree)) =>
            val res2 = transformParseTree(parseTree)(scope).right.map { newTree2 => newTree ++ file.map { newTree2.forFile(_) }.getOrElse(newTree2) }
            (res2, res.right.getOrElse(newTree))
        }._1
    }
  }
  
  def transform(s: String): Either[Seq[TransformerError], Tree] = transform(Map(), Some(parser.Parser.parse(s).get))(Tree(IntMap()))
}