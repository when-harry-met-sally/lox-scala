package bar
import utest.*
object BarTests extends TestSuite {
  def tests = Tests {
    test("hello") {
      val result = "bob"
      assert(result.startsWith("Hello"))
      result
    }
  }
}
