package pl.luckboy.combirian.interp
import scala.collection.immutable.BitSet
import scala.collection.immutable.IntMap
import scala.collection.immutable.Queue
import scala.util.parsing.input.NoPosition
import scala.annotation.tailrec

object Initializer
{
  def init[Env <: EnvironmentLike[Env]](tree: Tree)(eval: Evaluator[Env])(env: Env): Either[ErrorValue, Env] = {
    tree.combinatorBinds.foldLeft(Right((Nil, BitSet() | env.globalVarIdxs)): Either[ErrorValue, (List[Int], BitSet)]) {
      case (Right((idxs, markedIdxs)), (idx, _)) => dependenciesAndMarkedIdxs(idx, markedIdxs)(tree)
      case (Left(errValue), _)                   => Left(errValue)
    } match {
      case Right((idxs, _)) =>
        val preinitVarValues = tree.combinatorBinds.map {
          case (idx, CombinatorBind(_, Combinator(Seq(), _, _), _)) =>
            (idx, ErrorValue("initialization cycle", Seq()))
          case (idx, combBind)                                    =>
            (idx, CombinatorValue(idx, combBind))
        }.toMap
        val preinitEnv = env.withGlobalVars(preinitVarValues)
        idxs.foldLeft(Right(preinitEnv): Either[ErrorValue, Env]) {
          case (Right(env), idx) =>
            tree.combinatorBinds.get(idx).toRight(ErrorValue("undefined global variable", Seq())).right.flatMap {
              case combBind => 
                CombinatorValue(idx, combBind)(Seq())(eval)(env) match {
                  case errValue: ErrorValue => Left(errValue)
                  case value                => Right(env.withGlobalVars(Map(idx -> value)))
                }
            }
        }
    }
  }

  def dependenciesAndMarkedIdxs(idx: Int, markedIdxs: BitSet)(tree: Tree) =
    bfs(Queue(idx), Nil, markedIdxs, false)(tree)
  
  private def usedGlobalVarIdxsFromTerm(term: Term): BitSet =
    term match {
      case App(fun, args, _)        => 
        usedGlobalVarIdxsFromTerm(fun) ++ args.flatMap(usedGlobalVarIdxsFromTerm)
      case Let(binds, body, _)      => 
        BitSet() | binds.flatMap { bind => usedGlobalVarIdxsFromTerm(bind.body) }.toSet | usedGlobalVarIdxsFromTerm(body)
      case Lambda(_, _, body, _, _) =>
        usedGlobalVarIdxsFromTerm(body)
      case GlobalVar(idx, _)        =>
        BitSet(idx)
      case TailRecGlobalVar(idx, _) =>
        BitSet(idx)
      case _                        =>
        BitSet()
    }
 
  @tailrec
  private def bfs(q: Queue[Int], idxs: List[Int], markedIdxs: BitSet, mustEval: Boolean)(tree: Tree): Either[ErrorValue, (List[Int], BitSet)] =
    if(!q.isEmpty) {
      val (idx, newQ) = q.dequeue
      tree.combinatorBinds.get(idx) match {
        case Some(CombinatorBind(name, comb @ Combinator(_, body, _), _)) =>
          if(mustEval || comb.argCount == 0) {
            val invisitedNeighborIdxs = usedGlobalVarIdxsFromTerm(body).filterNot(markedIdxs.contains)
            val newIdxs = if(comb.argCount == 0) idx :: idxs else idxs
            bfs(newQ.enqueue(invisitedNeighborIdxs), newIdxs, markedIdxs | invisitedNeighborIdxs, true)(tree)
          } else
            Right(idxs, markedIdxs)
        case None =>
          Left(ErrorValue("undefined global variable", Seq()))
      }
    } else
      Right(idxs, markedIdxs)
}