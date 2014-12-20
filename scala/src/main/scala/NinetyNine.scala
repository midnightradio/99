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
}
