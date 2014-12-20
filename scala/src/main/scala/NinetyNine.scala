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
}
