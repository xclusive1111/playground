import org.scalatest.FunSuite
import ExcelApp._

class ExcelAppTests extends FunSuite {
  test("Cell name regex") {
    assert(isValidCellName("A1"))
    assert(isValidCellName("A1123"))
    assert(isValidCellName("B9"))

    assert(! isValidCellName("9B"))
    assert(! isValidCellName("BB9"))
    assert(! isValidCellName("B9B"))
    assert(! isValidCellName("B 9"))
    assert(! isValidCellName(" B9 "))
    assert(! isValidCellName("A")) // Missing column number
  }

  test("Formula regex") {
    assert(isValidFormula("A1"))
    assert(isValidFormula("D190"))
    assert(isValidFormula("15.")) // Equivalent to 15.0
    assert(isValidFormula("5.5"))
    assert(isValidFormula("10.0123"))
    assert(isValidFormula("5 6.1 -"))
    assert(isValidFormula("A1 6 +"))
    assert(isValidFormula("-A1 A2 +"))
    assert(isValidFormula("-A1 A2 + -5.1 *"))
    assert(isValidFormula("-B2 -4 + -1 /"))
    assert(isValidFormula("-B2 -4.9 + -A9 / C10 - D2112 *"))

    assert(! isValidFormula("1.1."))
    assert(! isValidFormula("15 6.0"))     // Missing operator
    assert(! isValidFormula("0 + A1"))
    assert(! isValidFormula("A9 6 + +"))    // Redundant '+' operator
    assert(! isValidFormula("9 6 + A3 ^"))  // Invalid operator
    assert(! isValidFormula("C 6 +"))       // Invalid cell identifier
    assert(! isValidFormula("+D1 -D2 *"))
    assert(! isValidFormula("D"))
    assert(! isValidFormula("120D"))
  }

  test("Posix regex") {
    assert(isPosixNotation("1 2.0 +"))
    assert(isPosixNotation("-11.1 2 +"))
    assert(isPosixNotation("9 -2 + 3 * -4 / -10 -"))

    assert(! isPosixNotation("+ +"))
    assert(! isPosixNotation("+1 +2 +"))
    assert(! isPosixNotation("1 + 2.5"))
    assert(! isPosixNotation("1 - 2"))
    assert(! isPosixNotation("1 -2 + -"))
    assert(! isPosixNotation(""))
    assert(! isPosixNotation("11.1. 5 -"))
  }
}
