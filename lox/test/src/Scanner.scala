package bar
import utest.*
import scanner.Scanner

object BarTests extends TestSuite {
  def tests = Tests {
    test("TODO") {
      val tokens = Scanner.scan("while 1.23 miles")
      assert(tokens.length == 4)
    }
  }
}
