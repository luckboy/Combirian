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
    
    it should "interpret a program with a condition" in {
      val s = "main = \\x -> if(x == \"yes\") (\"yes!\", nil) else (\"no!\", nil)"
      interpString(s, "yes") should be ===(Right("yes!\n"))
      interpString(s, "no") should be ===(Right("no!\n"))
    }
    
    it should "interpret a program with a partial application" in {
      val s = """
f x y z = x + y + z
g x = f x x
main = \x -> (g 10 (intfrom x), nil)
"""
      interpString(s, "5") should be ===(Right("25\n"))
      interpString(s, "100") should be ===(Right("120\n"))
    }
    
    it should "allocate local variables" in {
      val s = """
main = \x -> let
    x1 = x + x
    x2 = (intfrom x) * ((intfrom x) + 1)
  in
    let
      x3 = x1 + "!"
      x4 = x2 / 10
    in
      (x1 + "," + (stringfrom x2) + "," + x3 + "," + (stringfrom x4), nil)
"""
      interpString(s, "3") should be ===(Right("33,12,33!,1\n"))
      interpString(s, "10") should be ===(Right("1010,110,1010!,11\n"))
    }
    
    it should "create a copy of the array with a new element in non-lambda expression" in {
      interpString("""
main = \x -> let
    a = #[1, 2, 3, 4]
  in
    ((stringfrom a) + "," + (stringfrom (updated 1 10 a)) + "," + (stringfrom (updated 2 15 a)), nil)
""", "Some thing") should be ===(Right("#[1, 2, 3, 4],#[1, 10, 3, 4],#[1, 2, 15, 4]\n"))
    }

    it should "create a copy of the array with a new element in lambda expression" in {
      interpString("""
main = \x -> let
    a = #[1, 2, 3, 4]
  in
    let
      f = \v -> updated 2 v a 
    in
      ((stringfrom (f 10)) + "," + (stringfrom (f 15)), nil)
""", "Some thing") should be ===(Right("#[1, 2, 10, 4],#[1, 2, 15, 4]\n"))
    }
  }
  
  "A interpreter for an eager evaluator" should behave like interpreter(EagerEvaluator)(Environment)
}