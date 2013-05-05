package pl.luckboy.combirian.interp.spec
import java.io.BufferedReader
import java.io.StringReader
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.combirian.parser.Parser
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

    def interpStringForFile(s: String, in: String, file: java.io.File) = {
      val sr = new StringReader(in)
      val baos = new ByteArrayOutputStream()
      val br = new BufferedReader(sr)
      val ps = new PrintStream(baos)
      try {
        val tree = (for {
          parseTree <- Parser.parseString(s).right
          tree <- Transformer.transform(Map(file -> parseTree), None)(Tree.empty).right
        } yield { tree }).right.get
        //println(tree)
        Interpreter.interp(tree, br, ps)(eval)(factory).right.map { _ => baos.toString("UTF-8") }
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
    let
      a1 = updated 1 10 a
      a2 = updated 2 15 a
    in
      ((stringfrom a) + "," + (stringfrom a1) + "," + (stringfrom a2), nil)
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
      let
        a1 = f 10
        a2 = f 15
      in
        ((stringfrom a1) + "," + (stringfrom a2), nil)
""", "Some thing") should be ===(Right("#[1, 2, 10, 4],#[1, 2, 15, 4]\n"))
    }
    
    it should "create a copy of the array with a new element in partial application" in {
      interpString("""
main = \x -> let
    a = #[1, 2, 3, 4]
    f = \b v -> updated 3 v b
  in
    let
      g = f a
    in
      let
        a1 = g 101
        a2 = g 202
      in
        ((stringfrom a1) + "," + (stringfrom a2), nil)
""", "Some thing") should be ===(Right("#[1, 2, 3, 101],#[1, 2, 3, 202]\n"))
    }
    
    it should "interpret a tail recursion" in {
      val s = """
f n i v = if(i <= n) f n (i + 1) (v + i) else v
main = \x -> (f (intfrom x) 1 [], nil)
"""
      interpString(s, "5") should be ===(Right("[1, 2, 3, 4, 5]\n"))
      interpString(s, "10") should be ===(Right("[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n"))
    }

    it should "interpret a tail recursion for lambda combinator" in {
      val s = """
f = \n i v -> if(i <= n) f n (i + 1) (v + i) else v
main = \x -> (f (intfrom x) 1 [], nil)
"""
      interpString(s, "5") should be ===(Right("[1, 2, 3, 4, 5]\n"))
      interpString(s, "10") should be ===(Right("[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n"))
    }
    
    it should "read many lines" in {
      val s = "main = \\x -> (x + x, main)"
      interpString(s, "Line1\nLine2\nLine3\n") should be ===Right("Line1Line1\nLine2Line2\nLine3Line3\n")
      interpString(s, "Line1\nLine2\nLine3\nLine4") should be ===Right("Line1Line1\nLine2Line2\nLine3Line3\nLine4Line4\n")
    }
    
    it should "complain at the lambda expression" in {
      val stackTraceString = interpStringForFile("main = \\x -> 0 / 0", "Some thing", new java.io.File("some_file.comb")).left.map {
        _.stackTraceString
      }.left.get
      stackTraceString should be ===(
          "error: divide by zero\n" +
          "\tsome_file.comb: <lambda>: 1.14")
    }

    it should "complain at the nested lambda expression" in {
      val stackTraceString = interpStringForFile("main = \\x -> (\\y -> 0 / 0) x", "Some thing", new java.io.File("some_file.comb")).left.map {
        _.stackTraceString
      }.left.get
      stackTraceString should be ===(
          "error: divide by zero\n" +
          "\tsome_file.comb: <lambda>: 1.21\n" +
          "\tsome_file.comb: <lambda>: 1.14")
    }

    it should "complain at the combinator" in {
      val stackTraceString = interpStringForFile("""
main = \x -> f x
f x = 0 / 0
""", "Some thing", new java.io.File("some_file.comb")).left.map {
        _.stackTraceString
      }.left.get
      stackTraceString should be ===(
          "error: divide by zero\n" +
          "\tsome_file.comb: f: 3.7\n" +
          "\tsome_file.comb: <lambda>: 2.14")
    }
    
    it should "complain at the lambda expression at the combinator" in {
      val stackTraceString = interpStringForFile("""
main = \x -> f x x
f x = (\y -> 0 / 0) x
""", "Some thing", new java.io.File("some_file.comb")).left.map {
        _.stackTraceString
      }.left.get
      stackTraceString should be ===(
          "error: divide by zero\n" +
          "\tsome_file.comb: <lambda>: 3.14\n" +
          "\tsome_file.comb: f: 3.7\n" +
          "\tsome_file.comb: <lambda>: 2.14")
    }
  }
  
  "A interpreter for an eager evaluator" should behave like interpreter(EagerEvaluator)(Environment)
}