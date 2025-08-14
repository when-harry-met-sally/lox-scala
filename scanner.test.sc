//> using dep org.scalatest::scalatest:3.2.19
//> using file "./scanner.sc"

import org.scalatest.funsuite.AnyFunSuite

class ScannerTest extends AnyFunSuite {
  test("addition should add two numbers") {
    val text = "hi"
    val tokens = ""
    assert(scanner) == 5
  }

  test("multiplication should multiply two numbers") {
    assert(Calculator.multiply(2, 3) == 6)
  }
}
