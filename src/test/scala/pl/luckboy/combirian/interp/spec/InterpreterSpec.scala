package pl.luckboy.combirian.interp.spec
import java.io.BufferedReader
import java.io.StringReader
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.combirian.interp._

class InterpreterSpec[Env <: EnvironmentLike[Env]] extends FlatSpec with ShouldMatchers
{  
  def interpreter[Env <: EnvironmentLike[Env]](eval: Evaluator[Env])(factory: EnvironmentFactory[Env]): Unit = {
    def interpString(s: String, in: String) = {
      val sr = new StringReader(in)
      val baos = new ByteArrayOutputStream()
      val br = new BufferedReader(sr)
      val ps = new PrintStream(baos)
      try {
        Interpreter.interpString(s, br, ps)(eval)(factory).right.map { _ => baos.toString("UTF-8") }
      } finally {
        br.close()
        ps.close()
      }
    }
    
    it should "interpret a simple program" in {
      interpString("main = \\x -> (x + x, nil)", "Hello") should be ===(Right("HelloHello\n"))
    }
  }
  
  "A interpreter for an eager evaluator" should behave like interpreter(EagerEvaluator)(Environment)
}