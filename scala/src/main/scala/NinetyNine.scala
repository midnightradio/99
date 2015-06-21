object NinetyNine {
  def last[T](ls:List[T]):T = ls match {
    case h :: Nil => h
    case _ :: tail=> last(tail)
    case _        => throw new NoSuchElementException
  }

  def penultimate[T](ls:List[T]):T = ls match {
    case h :: _ :: Nil => h
    case _ :: tail     => penultimate(tail)
    case _             => throw new NoSuchElementException
  }

  def nth[T](n:Int, ls:List[T]):T = (n, ls) match {
    case (0, h :: _)    => h
    case (n, _ :: tail) => nth(n-1, tail) 
    case (_, Nil)       => throw new NoSuchElementException
  }

  def length[T](ls:List[T]):Int = ls match {
    case Nil       => 0
    case _ :: tail => 1 + length(tail)
  }

  def lengthTailRecursive[T](ls:List[T]):Int = {
    def lengthRecursive(acc:Int, currls:List[T]):Int = currls match {
      case Nil     => acc
      case _ :: tail => lengthRecursive(acc+1, tail)
    }
    lengthRecursive(0, ls)
  }

  def reverse[T](ls:List[T]):List[T] = {
    def reverseRecursive(acc:List[T], rest:List[T]):List[T] = acc match {
      case Nil => rest
      case h :: tail => reverseRecursive(tail, h::rest)
    }
    reverseRecursive(ls, Nil)
  }

  def isPalindrome[T](ls:List[T]):Boolean = {
    val m = ls.length / 2
    if (ls.take(m) == reverse(ls).take(m)) true else false
  }
}
