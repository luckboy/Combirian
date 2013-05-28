/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian.interp
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.io.BufferedReader
import scala.io.Source
import scala.annotation.tailrec
import pl.luckboy.combirian.AbstractError

object Interpreter
{
  case class Statistics(initTime: Long, evalTime: Long, ioTime: Long)
  
  @tailrec
  private def interpMainLoop[Env <: EnvironmentLike[Env]](funValue: Value, stdIn: BufferedReader, stdOut: PrintStream)(eval: Evaluator[Env])(env: Env)(stats: Statistics): (Statistics, Either[ErrorValue, Unit]) = {
	val readTime0 = System.currentTimeMillis()
    val line = stdIn.readLine()
    val readTime = System.currentTimeMillis() - readTime0
    
    val newStats = stats.copy(ioTime = stats.ioTime + readTime)
    if(line != null) {
      val evalTime0 = System.currentTimeMillis() 
      val retValue = funValue(Array(StringValue(line)))(eval)(env).force
      
      retValue match {
        case errValue: ErrorValue                        =>
          val evalTime = System.currentTimeMillis() - evalTime0
          val newStats2 = newStats.copy(evalTime = newStats.evalTime + evalTime)
          (newStats2, Left(errValue))
        case TupleValue(Seq(outLineValue, nextFunValue)) =>
          val evaluatedOutLineValue = outLineValue.force
          val outLine = evaluatedOutLineValue match {
            case StringValue(s) => s
            case _              => evaluatedOutLineValue.toString
          }
          val evalTime = System.currentTimeMillis() - evalTime0

          val writeTime0 = System.currentTimeMillis()
          stdOut.println(outLine)
          val writeTime = System.currentTimeMillis() - writeTime0
          
          val newStats2 = newStats.copy(evalTime = newStats.evalTime + evalTime, ioTime = newStats.ioTime + writeTime)
          if(nextFunValue != NilValue)
        	interpMainLoop(nextFunValue, stdIn, stdOut)(eval)(env)(newStats2)
          else
            (newStats2, Right(()))
        case outLineValue                                =>
          val evalTime = System.currentTimeMillis() - evalTime0
          val newStats2 = newStats.copy(evalTime = newStats.evalTime + evalTime)
          (newStats2, Left(ErrorValue("illegal result", Seq())))
      }
    } else
      (newStats, Right(()))
  }
  
  def interp[Env <: EnvironmentLike[Env]](tree: Tree, stdIn: BufferedReader, stdOut: PrintStream)(eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): (Statistics, Either[ErrorValue, Unit]) = {
    val initTime0 = System.currentTimeMillis()
    val initRes = Initializer.init(tree)(eval)(factory.empty) 
    val initTime = System.currentTimeMillis() - initTime0
    
    val stats = Statistics(initTime = initTime, evalTime = 0L, ioTime = 0L)
    initRes match {
      case Right(newEnv)  => 
        tree.combinatorBinds.find { case (_, combBind) => combBind.name == "main" }.map {
          case (idx, combBind) => 
            newEnv.globalVarValue(idx) match {
              case errValue: ErrorValue => (stats, Left(errValue))
              case funValue             => interpMainLoop(funValue, stdIn, stdOut)(eval)(newEnv.withCurrentFile(combBind.file))(stats)
            }
        }.getOrElse((stats, Left(ErrorValue("undefined global variable main", Seq()))))
      case Left(errValue) =>
        (stats, Left(errValue))
    }
  }

  def interpString[Env <: EnvironmentLike[Env]](s: String, in: BufferedReader, out: PrintStream)(eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): Either[Either[Seq[AbstractError], ErrorValue], Unit] =
    Transformer.transformString(s)(Tree.empty) match {
      case Right(tree) => interp(tree, in, out)(eval)(factory)._2.left.map { Right(_) }
      case Left(errs)  => Left(Left(errs))
    }

  def interpInputStream[Env <: EnvironmentLike[Env]](in: InputStream, stdIn: BufferedReader, stdOut: PrintStream)(eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): Either[Either[Seq[AbstractError], ErrorValue], Unit] =
    Transformer.transformInputStream(in)(Tree.empty) match {
      case Right(tree) => interp(tree, stdIn, stdOut)(eval)(factory)._2.left.map { Right(_) }
      case Left(errs)  => Left(Left(errs))
    }

  def interpFile[Env <: EnvironmentLike[Env]](file: java.io.File, stdIn: BufferedReader, stdOut: PrintStream)(eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): Either[Either[Seq[AbstractError], ErrorValue], Unit] =
    Transformer.transformFile(file)(Tree.empty) match {
      case Right(tree) => interp(tree, stdIn, stdOut)(eval)(factory)._2.left.map { Right(_) }
      case Left(errs)  => Left(Left(errs))
    }
  
  def interpFiles[Env <: EnvironmentLike[Env]](files: Set[java.io.File], stdIn: BufferedReader, stdOut: PrintStream)(eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): Either[Either[Seq[AbstractError], ErrorValue], Unit] =
    Transformer.transformFiles(files)(Tree.empty) match {
      case Right(tree) => interp(tree, stdIn, stdOut)(eval)(factory)._2.left.map { Right(_) }
      case Left(errs)  => Left(Left(errs))
    }
}
