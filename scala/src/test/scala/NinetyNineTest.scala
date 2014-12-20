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
}
