/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian.interp
import scala.collection.immutable.BitSet
import scala.collection.immutable.IntMap
import scala.collection.immutable.Stack
import scala.util.parsing.input.NoPosition
import scala.annotation.tailrec

object Initializer
{
  def init[Env <: EnvironmentLike[Env]](tree: Tree)(eval: Evaluator[Env])(env: Env): Either[ErrorValue, Env] = {
    tree.combinatorBinds.foldLeft(Right((Nil, BitSet() | env.globalVarIdxs)): Either[ErrorValue, (List[Int], BitSet)]) {
      case (Right((idxs, markedIdxs)), (idx, _)) =>
        dependenciesAndMarkedIdxs(idx, markedIdxs)(tree) match {
          case Right((tmpIdxs, newMarkedIdxs)) => Right((idxs ++ tmpIdxs, newMarkedIdxs))
          case Left(errValue)                  => Left(errValue)
        }
      case (Left(errValue), _)                   =>
        Left(errValue)
    } match {
      case Right((idxs, _)) =>
        val preinitVarValues = tree.combinatorBinds.map {
          case (idx, CombinatorBind(name, Combinator(Seq(), _, _), _)) =>
            (idx, ErrorValue("initialization cycle", Seq()))
          case (idx, combBind)                                    =>
            (idx, CombinatorValue(idx, combBind))
        }.toMap
        val preinitEnv = env.withGlobalVars(preinitVarValues)
        idxs.foldLeft(Right(preinitEnv): Either[ErrorValue, Env]) {
          case (Right(env), idx)   =>
            tree.combinatorBinds.get(idx).toRight(ErrorValue("undefined global variable", Seq())).right.flatMap {
              case combBind => 
                CombinatorValue(idx, combBind)(Seq())(eval)(env.withCurrentFile(combBind.file)) match {
                  case errValue: ErrorValue => Left(errValue)
                  case value                => Right(env.withGlobalVars(Map(idx -> value)))
                }
            }
          case (Left(errValue), _) =>
            Left(errValue)
        }
    }
  }

  def dependenciesAndMarkedIdxs(idx: Int, markedIdxs: BitSet)(tree: Tree) =
    tree.combinatorBinds.get(idx).map {
      case CombinatorBind(name, comb @ Combinator(_, body, _), _) =>
        if(comb.argCount == 0 && !markedIdxs.contains(idx)) {
          val neighborIdxs = usedGlobalVarIdxsFromTerm(body).filterNot(markedIdxs.contains)
          dfs(Stack(idx -> neighborIdxs.toList), Nil, markedIdxs + idx)(tree).right.map {
            case (idxs, markedIdxs) => (idxs.reverse, markedIdxs)
          }
        } else
          Right((Nil, markedIdxs))
    }.getOrElse(Left(ErrorValue("undefined global variable", Seq())))
  
  private def usedGlobalVarIdxsFromTerm(term: Term): BitSet =
    term match {
      case App(fun, args, _)           => 
        usedGlobalVarIdxsFromTerm(fun) ++ args.flatMap(usedGlobalVarIdxsFromTerm)
      case Let(binds, body, _)         => 
        BitSet() | binds.flatMap { bind => usedGlobalVarIdxsFromTerm(bind.body) }.toSet | usedGlobalVarIdxsFromTerm(body)
      case Lambda(_, _, body, _, _, _) =>
        usedGlobalVarIdxsFromTerm(body)
      case GlobalVar(idx, _)           =>
        BitSet(idx)
      case TailRecGlobalVar(idx, _)    =>
        BitSet(idx)
      case _                           =>
        BitSet()
    }
 
  @tailrec
  private def dfs(stck: Stack[(Int, List[Int])], idxs: List[Int], markedIdxs: BitSet)(tree: Tree): Either[ErrorValue, (List[Int], BitSet)] =
    if(!stck.isEmpty) {
      val ((idx, neighborIdxs), newStck) = stck.pop2
      neighborIdxs match {
        case neighborIdx :: newNeighborIdxs =>
          if(!markedIdxs.contains(neighborIdx))
            tree.combinatorBinds.get(neighborIdx) match {
              case Some(CombinatorBind(name, Combinator(_, body, _), _)) =>
                val neighborIdxs2 = usedGlobalVarIdxsFromTerm(body)
                val newStck2 = newStck.push(idx -> newNeighborIdxs).push(neighborIdx -> neighborIdxs2.toList)
                dfs(newStck2, idxs, markedIdxs + neighborIdx)(tree)
              case None =>
                Left(ErrorValue("undefined global variable", Seq()))
            }
          else
            dfs(newStck.push(idx -> newNeighborIdxs), idxs, markedIdxs + neighborIdx)(tree)
        case Nil                            =>
          tree.combinatorBinds.get(idx) match {
            case Some(CombinatorBind(name, comb @ Combinator(_, body, _), _)) =>
              dfs(newStck, if(comb.argCount == 0) idx :: idxs else idxs, markedIdxs)(tree)
            case None                                                         =>
              Left(ErrorValue("undefined global variable", Seq()))
          }
      }
    } else
      Right(idxs, markedIdxs)
}
