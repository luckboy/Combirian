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
  @tailrec
  private def interpMainLoop[Env <: EnvironmentLike[Env]](funValue: Value, stdIn: BufferedReader, stdOut: PrintStream)(eval: Evaluator[Env])(env: Env): Either[ErrorValue, Unit] = {
    val line = stdIn.readLine()
    if(line != null) {
      funValue(Array(StringValue(line)))(eval)(env) match {
        case errValue: ErrorValue                        =>
          Left(errValue)
        case TupleValue(Seq(outLineValue, nextFunValue)) =>
          outLineValue match {
            case StringValue(s) => stdOut.println(s)
            case _              => stdOut.println(outLineValue.toString)
          }
          if(nextFunValue != NilValue)
        	interpMainLoop(nextFunValue, stdIn, stdOut)(eval)(env)
          else
            Right(())
        case outLineValue                                =>
          Left(ErrorValue("illegal result", Seq()))
      }
    } else
      Right(())
  }
  
  def interp[Env <: EnvironmentLike[Env]](tree: Tree, stdIn: BufferedReader, stdOut: PrintStream)(eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): Either[ErrorValue, Unit] =
    Initializer.init(tree)(eval)(factory.empty) match {
      case Right(newEnv)  => 
        tree.combinatorBinds.find { case (_, combBind) => combBind.name == "main" }.map {
          case (idx, combBind) => 
            newEnv.globalVarValue(idx) match {
              case errValue: ErrorValue => Left(errValue)
              case funValue             => interpMainLoop(funValue, stdIn, stdOut)(eval)(newEnv.withCurrentFile(combBind.file))
            }
        }.getOrElse(Left(ErrorValue("undefined global variable main", Seq())))
      case Left(errValue) =>
        Left(errValue)
    }

  def interpString[Env <: EnvironmentLike[Env]](s: String, in: BufferedReader, out: PrintStream)(eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): Either[Either[Seq[AbstractError], ErrorValue], Unit] =
    Transformer.transformString(s)(Tree.empty) match {
      case Right(tree) => interp(tree, in, out)(eval)(factory).left.map { Right(_) }
      case Left(errs)  => Left(Left(errs))
    }

  def interpInputStream[Env <: EnvironmentLike[Env]](in: InputStream, stdIn: BufferedReader, stdOut: PrintStream)(eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): Either[Either[Seq[AbstractError], ErrorValue], Unit] =
    Transformer.transformInputStream(in)(Tree.empty) match {
      case Right(tree) => interp(tree, stdIn, stdOut)(eval)(factory).left.map { Right(_) }
      case Left(errs)  => Left(Left(errs))
    }

  def interpFile[Env <: EnvironmentLike[Env]](file: java.io.File, stdIn: BufferedReader, stdOut: PrintStream)(eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): Either[Either[Seq[AbstractError], ErrorValue], Unit] =
    Transformer.transformFile(file)(Tree.empty) match {
      case Right(tree) => interp(tree, stdIn, stdOut)(eval)(factory).left.map { Right(_) }
      case Left(errs)  => Left(Left(errs))
    }
  
  def interpFiles[Env <: EnvironmentLike[Env]](files: Set[java.io.File], stdIn: BufferedReader, stdOut: PrintStream)(eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): Either[Either[Seq[AbstractError], ErrorValue], Unit] =
    Transformer.transformFiles(files)(Tree.empty) match {
      case Right(tree) => interp(tree, stdIn, stdOut)(eval)(factory).left.map { Right(_) }
      case Left(errs)  => Left(Left(errs))
    }
}