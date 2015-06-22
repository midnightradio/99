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

  test("P05 (*) Reverse a list.") {
    NinetyNine.reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
  }

  test("P06 (*) Find out whether a list is a palindrome.") {
    isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
    isPalindrome(List("x", "a", "m", "a", "x")) should be (true)
    isPalindrome(List(1, 2, 3)) should be (false)
  }

  test("P07 (**) Flatten a nested list structure.") {
    flatten(List("a", List("b", List("c", "d"), "e"))) should be (List("a", "b", "c", "d", "e"))
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be (List(1, 1, 2, 3, 5, 8))
  }

  test("P08 (**) Eliminate consecutive duplicates of list elements.") {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
    compressTailRecursive(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("P09 (**) Pack consecutive duplicates of list elements into sublists.") {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("P10 (*) Run-length encoding of a list.") {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("P11 (*) Modified run-length encoding.") {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  test("P12 (**) Decode a run-length encoded list.") {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  test("P13 (**) Run-length encoding of a list (direct solution).") {
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("P14 (*) Duplicate the elements of a list.") {
    duplicate(List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("P15 (**) Duplicate the elements of a list a given number of times.") {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("P16 (**) Drop every Nth element from a list.") {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("P17 (*) Split a list into two parts.") {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be ((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}
