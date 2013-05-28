/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian.interp
import scala.collection.immutable.IntMap
import scala.util.parsing.input.Position
import pl.luckboy.combirian.parser
import pl.luckboy.combirian.AbstractError
import pl.luckboy.combirian.ResultUtils._

object Transformer
{
  case class Scope(
      globalVarIdxs: Map[String, Int],
      localVarIdxs: Map[String, Int], 
      localVarRefCounts: Map[String, Int],
      currentTailRecInfo: Option[TailRecInfo],
      currentCombinatorIdx: Option[Int])
  {
    def withLocalVarIdxs(idxs: Iterable[(String, Int)]) = copy(localVarIdxs = localVarIdxs ++ idxs)

    def withGlobalVarIdxs(idxs: Iterable[(String, Int)]) = copy(globalVarIdxs = globalVarIdxs ++ idxs)
  }
  
  object Scope
  {
    val empty = Scope(
        globalVarIdxs = Map(),
        localVarIdxs = Map(),
        localVarRefCounts = Map(),
        currentTailRecInfo = None,
        currentCombinatorIdx = None)
  }
  
  case class TailRecInfo(name: String, argCount: Int)
    
  private def closureVarIdxsFromTerm(term: parser.Term)(localVarIdxs: Map[String, Int]): Map[String, Int] =
    term match {
      case parser.App(fun, args)     =>
        closureVarIdxsFromTerm(fun)(localVarIdxs) ++ args.flatMap { closureVarIdxsFromTerm(_)(localVarIdxs) }
      case parser.Let(binds, body)   =>
        closureVarIdxsFromTerm(body)(localVarIdxs -- binds.map { _.name })
      case parser.Lambda(args, body) =>
        closureVarIdxsFromTerm(body)(localVarIdxs -- args.map { _.name })
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
    counts1.map { case (name, count1) => (name, count1.max(counts2.getOrElse(name, 0))) } ++ (counts2 -- counts1.keys)

  private def sumLocalVarRefCounts(counts1: Map[String, Int], counts2:  Map[String, Int]): Map[String, Int] =
    counts1.map { case (name, count1) => (name, count1 + counts2.getOrElse(name, 0)) } ++ (counts2 -- counts1.keys)
  
  def transformTerms(terms: Seq[parser.Term])(scope: Scope): Either[Seq[TransformerError], Seq[Term]] =
    terms.foldLeft(Right(Nil): Either[Seq[TransformerError], Seq[Term]]) {
      (res, term) => zipResults(res, transformTerm(term, false)(scope)).right.map { case (terms2, term2) => terms2 :+ term2 }
    }
  
  private def app(fun: parser.Term, args: Seq[parser.Term], pos: Position)(scope: Scope) =
    zipResults(transformTerm(fun, false)(scope), transformTerms(args)(scope)).right.map {
      case (fun2, args2) => App(fun2, args2, pos)
    }
  
  private def lambda(args: Seq[parser.Arg], body: parser.Term, pos: Position, tailRecInfo: Option[TailRecInfo])(scope: Scope) = {
    val closureVarIdxs = closureVarIdxsFromTerm(body)(scope.localVarIdxs -- args.map { _.name })
    val tmpLocalVarRefCounts1 = scope.localVarRefCounts -- (scope.localVarRefCounts.keySet -- closureVarIdxs.keySet)
    val tmpLocalVarRefCounts2 = localVarRefCountsFromTerm(body)(args.map { _.name }.toSet)(args.map { arg => (arg.name, 0) }.toMap)    
    val newLocalVarRefCounts = sumLocalVarRefCounts(tmpLocalVarRefCounts1, tmpLocalVarRefCounts2)
    val closureVarNamesAndIndexes = closureVarIdxs.toSeq
    val closureVarIndexes = closureVarNamesAndIndexes.map { _._2 }
    val tmpScope = scope.copy(
        localVarIdxs = closureVarNamesAndIndexes.zipWithIndex.map { case ((name, _), newIdx) => (name, newIdx) }.toMap, 
        localVarRefCounts = newLocalVarRefCounts,
        currentTailRecInfo = tailRecInfo)
    val argIdxs = args.zipWithIndex.map { case (arg, i) => (arg.name, i + tmpScope.localVarIdxs.size) }
    val newScope = tmpScope.withLocalVarIdxs(argIdxs)
    transformTerm(body, tailRecInfo.isDefined)(newScope).right.map { 
      Lambda(closureVarIndexes, args.map { _.name }, _, localVarCountFromTerm(body), scope.currentCombinatorIdx.get, pos) 
    }
  }
  
  def transformTermForTailRec(term: parser.Term, canTailRec: Boolean, argCount: Int)(scope: Scope): Either[Seq[TransformerError], Term] =
    term match {
      case parser.Lambda(args, body) if canTailRec  && args.size == argCount =>
        lambda(args, body, term.pos, scope.currentTailRecInfo)(scope)
      case _                                                                 =>
        transformTerm(term, canTailRec)(scope)
    }
  
  def transformTerm(term: parser.Term, canTailRec: Boolean)(scope: Scope): Either[Seq[TransformerError], Term] =
    term match {
      case parser.App(fun @ parser.Var(funName), args) if canTailRec =>
        scope.currentTailRecInfo.map {
          tailRecInfo =>
            if(tailRecInfo.name == funName && !scope.localVarIdxs.contains(funName) && tailRecInfo.argCount == args.size)
              transformTerms(args)(scope).right.flatMap {
                args2 =>
                  scope.globalVarIdxs.get(tailRecInfo.name).map {
                    idx => Right(App(TailRecGlobalVar(idx, fun.pos), args2, term.pos))
                  }.getOrElse(Left(Seq(TransformerError(None, term.pos, "undefined variable " + funName))))                  
              }
            else
              app(fun, args, term.pos)(scope)
        }.getOrElse(app(fun, args, term.pos)(scope))
      case parser.App(fun @ parser.Literal(BuiltinFunValue(BuiltinFunction.Cond)), Seq(firstTerm, secondTerm, condTerm)) =>
        zipResults(
            transformTermForTailRec(firstTerm, canTailRec, 1)(scope), 
            zipResults(
                transformTermForTailRec(secondTerm, canTailRec, 1)(scope), 
                transformTerm(condTerm, false)(scope))).right.map {
          case (firstTerm2, (secondTerm2, condTerm2)) =>
            App(Literal(BuiltinFunValue(BuiltinFunction.Cond), fun.pos), Seq(firstTerm2, secondTerm2, condTerm2), term.pos)
        }
      case parser.App(fun @ parser.Literal(BuiltinFunValue(BuiltinFunction.Uncurry)), Seq(otherTerm, tuple)) =>
        zipResults(
            transformTermForTailRec(otherTerm, canTailRec, 2)(scope),
            transformTerm(tuple, false)(scope)
            ).right.map {
          case (otherTerm2, tuple2) =>
            App(Literal(BuiltinFunValue(BuiltinFunction.Uncurry), fun.pos), Seq(otherTerm2, tuple2), term.pos)
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
  
  def scopeFromTree(tree: Tree) = 
    Scope(
        globalVarIdxs = tree.combinatorBinds.map { case (idx, combBind) => (combBind.name, idx) },
        localVarIdxs = Map(),
        localVarRefCounts = Map(),
        currentTailRecInfo = None,
        currentCombinatorIdx =None)
  
  def transformParseTree(parseTree: parser.ParseTree)(scope: Scope): Either[Seq[TransformerError], Tree] =
    parseTree.defs.foldLeft(Right(IntMap()): Either[Seq[TransformerError], IntMap[CombinatorBind]]) {
      case (res, definition @ parser.Def(name, args, body)) =>
        val argIdxs = args.zipWithIndex.map { case (arg, idx) => (arg.name, idx) }.toMap
        val localVarRefCounts = localVarRefCountsFromTerm(body)(args.map { _.name }.toSet)(args.map { arg => (arg.name, 0) }.toMap)
        val idx = scope.globalVarIdxs(name)
        val res2 = body match {
          case parser.Lambda(lambdaArgs, lambdaBody) if args.isEmpty =>
            val tailRecInfo = TailRecInfo(name, lambdaArgs.size)
            val newScope = scope.copy(
                localVarIdxs = argIdxs, 
                localVarRefCounts = localVarRefCounts, 
                currentTailRecInfo = Some(tailRecInfo),
                currentCombinatorIdx = Some(idx))
            lambda(lambdaArgs, lambdaBody, body.pos, Some(tailRecInfo))(newScope)
          case _                                                     =>
            val newScope = scope.copy(
                localVarIdxs = argIdxs, 
                localVarRefCounts = localVarRefCounts, 
                currentTailRecInfo = Some(TailRecInfo(name, args.size)),
                currentCombinatorIdx = Some(idx))
            transformTerm(body, true)(newScope)
        }
        zipResults(res, res2).right.map {
          case (combBinds, body2) =>            
            (combBinds + (idx -> CombinatorBind(name, Combinator(args.map { _.name }, body2, localVarCountFromTerm(body)), None)))
        }
    }.right.map(Tree.apply).left.map { errs => errs.sortWith { (err1, err2) => err1.pos < err2.pos } }
    
  def transform(parseTrees: Map[java.io.File, parser.ParseTree], cmdParseTree: Option[parser.ParseTree])(tree: Tree): Either[Seq[TransformerError], Tree] = {
    val allParseTrees = parseTrees.map { case (file, parseTree) => (Some(file), parseTree) } ++ cmdParseTree.map { (None, _) }
    allParseTrees.foldLeft(Right(scopeFromTree(tree)): Either[Seq[TransformerError], Scope]) {
      case (Right(newScope), (file, parseTree)) => scopeFromParseTree(parseTree)(newScope)
      case (Left(errs), _)                      => Left(errs)
    }.right.flatMap {
      scope =>
        allParseTrees.foldLeft((Right(Tree(IntMap())), Tree(IntMap())): (Either[Seq[TransformerError], Tree], Tree)) {
          case ((res, newTree), (file, parseTree)) =>
            val res2 = transformParseTree(parseTree)(scope).right.map { newTree2 => newTree ++ file.map { newTree2.forFile(_) }.getOrElse(newTree2) }
            (res2, res.right.getOrElse(newTree))
        }._1
    }
  }
  
  def transformString(s: String)(tree: Tree): Either[Seq[AbstractError], Tree] = 
    for {
      parseTree <- parser.Parser.parseString(s).right
      newTree <- transform(Map[java.io.File, parser.ParseTree](), Some(parseTree))(tree).right
    } yield newTree
    
  def transformInputStream(in: java.io.InputStream)(tree: Tree): Either[Seq[AbstractError], Tree] =
    for {
      parseTree <- parser.Parser.parseInputStream(in).right
      newTree <- transform(Map[java.io.File, parser.ParseTree](), Some(parseTree))(tree).right
    } yield newTree

  def transformFile(file: java.io.File)(tree: Tree): Either[Seq[AbstractError], Tree] =
    for {
      parseTree <- parser.Parser.parseFile(file).right
      newTree <- transform(Map(file -> parseTree), None)(tree).right
    } yield newTree

  def transformFiles(files: Set[java.io.File])(tree: Tree): Either[Seq[AbstractError], Tree] =
    for {
      parseTrees <- files.foldLeft(Right(Map()): Either[Seq[AbstractError], Map[java.io.File, parser.ParseTree]]) {
        case (res, file) =>
          zipResults(res, parser.Parser.parseFile(file)).right.map { case (newParseTrees, parseTree) => newParseTrees + (file -> parseTree) }
      }.right
      newTree <- transform(parseTrees, None)(tree).right
    } yield newTree
}
