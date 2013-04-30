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
    
    it should "initialize all variables" in {
      interpString("""
main = \x -> (g1 + g2 + g3 + g4, nil)
f2 x y = (f1 (x * x) (y * y)) + g2
g1 = g2 // 5 
g2 = 5  // 5
g3 = f1 g1 3 // 5 + 3 = 8
f1 x y = x + y
g4 = f2 10 4 // (10 * 10) + (4 * 4) + 5 = 121
""", "Some thing") should be ===(Right("139\n"))
    }
  }
  
  "A interpreter for an eager evaluator" should behave like interpreter(EagerEvaluator)(Environment)
}