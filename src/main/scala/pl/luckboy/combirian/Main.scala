/*******************************************************************************
 * Copyright (c) 2013 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.combirian
import java.io.PrintStream
import java.io.BufferedReader
import pl.luckboy.combirian.parser.Parser
import pl.luckboy.combirian.interp.Transformer
import pl.luckboy.combirian.interp.Interpreter
import pl.luckboy.combirian.interp.Tree
import pl.luckboy.combirian.interp.EagerEvaluator
import pl.luckboy.combirian.interp.LazyEvaluator
import pl.luckboy.combirian.interp.Environment

object Main
{
  private object Opt1 extends Enumeration
  {
    val Verbose = Value ("-v")
    val Time = Value("-t")
    val Help = Value("-h")
  }
  
  private object Opt2 extends Enumeration
  {
    val Evaluator = Value("-e")
  }
  
  private val interps = Map(
      "eager" -> { Interpreter.interp(_: Tree, _: BufferedReader, _: PrintStream)(EagerEvaluator)(Environment) },
      "lazy" -> { Interpreter.interp(_: Tree, _: BufferedReader, _: PrintStream)(LazyEvaluator)(Environment) }
      )
  
  private def parseOpts(args: List[String], opts1: Set[Opt1.Value], opts2: Map[Opt2.Value, String], fileNames: Set[String]): (Set[Opt1.Value], Map[Opt2.Value, String], Set[String]) =
    args match {
      case "--" :: nextArgs =>
        (opts1, opts2, fileNames | nextArgs.toSet)
      case arg :: optArg :: nextArgs if Opt2.values.exists { _.toString == arg } =>
        parseOpts(nextArgs, opts1, opts2 + (Opt2.withName(arg) -> optArg), fileNames)
      case arg :: nextArgs if Opt1.values.exists { _.toString == arg } =>
        parseOpts(nextArgs, opts1 + Opt1.withName(arg), opts2, fileNames)
      case arg :: nextArgs =>
        parseOpts(nextArgs, opts1, opts2, fileNames + arg)
      case Nil =>
        (opts1, opts2, fileNames)
    }
  
  def main(args: Array[String]): Unit = {
    val (opts1, opts2, fileNames) = parseOpts(args.toList, Set(), Map(), Set())
    if(opts1.contains(Opt1.Help)) {
      println("Usage: combirian [<option> ...] <file> ...")
      println("Options:")
      println("  -e <evaluator>       choose evaluator (default: eager)")
      println("  -t                   display times of interpretation")
      println("  -v                   display details of interpretation")
      println("  -h                   display this text")
    } else {
      if(!fileNames.isEmpty) {
        val evalName = opts2.getOrElse(Opt2.Evaluator, "eager")
        if(opts1.contains(Opt1.Verbose)) {
    	  println("Evaluator name: " + evalName); println()
        }
        interps.get(evalName).map {
          interp =>
            (for {
              parseTrees <- Parser.parseFiles(fileNames.map { new java.io.File(_) }.toSet).right.map {
                parseTrees =>
                  if(opts1.contains(Opt1.Verbose)) {
                    println("Parse trees:")
                    for((file, parseTree) <- parseTrees) { println("// " + file.getPath() + "\n"); println(parseTree) }
                    println()
                  }
                  parseTrees
              }.right
              tree <- Transformer.transform(parseTrees, None)(Tree.empty).right.map {
                tree =>
                  if(opts1.contains(Opt1.Verbose)) {
                    println("Tree:"); println(tree); println()
                  }
                  tree
              }.right
            } yield {
              if(opts1.contains(Opt1.Verbose)) {
                println("Interpreter input/output:")
              }
              val (stats, res) = interp(tree, Console.in, Console.out)
              res match {
                case Right(())      => ()
                case Left(errValue) => Console.err.println(errValue.stackTraceString)
              }
              if(opts1.contains(Opt1.Time)) {
                println()
                println("Time of initialization: " + stats.initTime + "ms")
                println("Time of evaluation:     " + stats.evalTime + "ms")
                println("Time of I/O operations: " + stats.ioTime + "ms")
              }
              res match {
                case Right(())      => ()
                case Left(errValue) => sys.exit(1)
              }
            }) match {
              case Right(())  => ()
              case Left(errs) => for(err <- errs) Console.err.println(err); sys.exit(1)
            }
        }.getOrElse {
          Console.err.println("unknown evaluator")
          sys.exit(1)
        }
      } else {
        Console.err.println("no file")
        sys.exit(1)
      }
    }
  }
}
