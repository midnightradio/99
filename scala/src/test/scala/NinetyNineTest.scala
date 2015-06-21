import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.Matchers

class NinetyNineTest extends FunSuite with Matchers {
  import NinetyNine._

  test("P01 (*) Find the last element of a list.") {
    last(List(1, 1, 2, 3, 5, 8)) should be (8)
  }

  test("P02 (*) Find the last but one element of a list.") {
    penultimate(List(1, 1, 2, 3, 5, 8)) should be (5)
  }

  test("P03 (*) Find the Kth element of a list.") {
    nth(2, List(1, 1, 2, 3, 5, 8)) should be (2)
  }

  test("P04 (*) Find the number of elements of a list.") {
    NinetyNine.length(List(1, 1, 2, 3, 5, 8)) should be (6)
    NinetyNine.lengthTailRecursive(List(1, 1, 2, 3, 5, 8)) should be (6)
  }
}
