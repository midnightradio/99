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

  /* P07 */
  def flatten(ls:List[Any]):List[Any] = {
    def flattenRecursive(tail:List[Any], acc:List[Any]):List[Any] = tail match {
        case Nil => acc
        /*
        case y :: Nil => y match {
            case x :: xs => flattenRecursive(xs, x :: acc)
            case x => flattenRecursive(Nil, x :: acc)
        }
        */
        case y :: ys => y match {
            case x :: Nil => {
                println(x+"::Nil")
                flattenRecursive(ys, x :: acc)
            }
            case x :: xs => {
                println(x+"::xs")
                flattenRecursive(xs, x :: acc) /* nested List */
            }
            case x => {
                println(x)
                flattenRecursive(ys, x :: acc)
            }
        }
    }
    flattenRecursive(ls, Nil).reverse
  }

  /* PO8 */
  def compress[T](ls:List[T]):List[T] = ls match {
    case Nil => Nil
    case x :: xs => x :: compress(xs.dropWhile(_ == x))
  }

  def compressTailRecursive[T](ls:List[T]):List[T] = {
    def compressRecursive(rest:List[T], result:List[T]):List[T] = rest match {
        case Nil => reverse(result)
        case x :: xs => compressRecursive(xs.dropWhile(_ == x), x :: result)
    }
    compressRecursive(ls, Nil)
  }

  /* P09 */
  def pack[T](ls:List[T]):List[List[T]] = ls match {
    case Nil => Nil
    case x :: xs => ls.takeWhile(_ == x) :: pack(ls.dropWhile(_ == x))
  }

  def packTailRecursive[T](ls:List[T]):List[List[T]] = {
    def packRecursive(rest:List[T], result:List[List[T]]):List[List[T]] = rest match {
        case Nil => reverse(result)
        case x :: xs => packRecursive(rest.dropWhile(_ == x), rest.takeWhile(_ == x) :: result)
    }
    packRecursive(ls, Nil)
  }

  /* P10 */
  def encode[T](ls:List[T]):List[(Int,T)] = {
    packTailRecursive(ls).map(l => Tuple2(l.size, l(0)))
  }

  /* P11 */
  def encodeModified[T](ls:List[T]):List[Any] = {
    encode(ls).map (t => if (t._1 == 1) t._2 else t)
  }

  /* P12 */
  def decode[T](ls:List[(Int, T)]):List[T] = {
    def repeat(ntimes:Int, elm:T):List[T] = {
        if (ntimes == 0) Nil else elm :: repeat(ntimes-1, elm)
    }
    ls.map(t => repeat(t._1, t._2)).flatten
  }

  /* P13 */
  def encodeDirect(ls:List[Any]):List[Any] = {
    def encodeDirectCount(acc:Int, ls:List[List[Any]], result:List[(Int, Any)]):List[(Int, Any)] = ls match {
      case Nil => reverse(result)
      case x :: Nil => encodeDirectCount(0, Nil, (acc+1, x(0)) :: result)
      case x :: xs => {
        if (x(0) == x(1) && xs != Nil) encodeDirectCount(acc+1, xs, result)
        else encodeDirectCount(1, xs, (acc, x(0)) :: result)
      }
    }
    encodeDirectCount(1, ls.sliding(2).toList, Nil)
  }

  /* P14 */
  def duplicate(ls:List[Any]):List[Any] = {
    ls.map(x => List(x, x)).flatten
  }

  /* P15 */
  def duplicateN(n:Int, ls:List[Any]):List[Any] = {
    def repeat(ntimes:Int, elm:Any):List[Any] = {
        if (ntimes == 0) Nil else elm :: repeat(ntimes-1, elm)
    }
    ls.map(x => repeat(n, x)).flatten
  }

  /* P16 */
  def drop(n:Int, ls:List[Any]):List[Any] = {
    def dropRecursive(acc:Int, tail:List[Any], result:List[Any]):List[Any] = tail match {
      case Nil => result.reverse
      case x :: xs => {
        if (acc == 1) dropRecursive(n, xs, result)
        else dropRecursive(acc-1, xs, x :: result)
      }
    }
    dropRecursive(n, ls, Nil)
  }

  /* P17 */
  def split(n:Int, ls:List[Any]):Tuple2[List[Any], List[Any]] = {
    Tuple2(ls.take(n), ls.drop(3))
  }

  /* P18 */
  def slice(s:Int, e:Int, ls:List[Any]):List[Any] = {
    Range(s,e).map(i => ls(i)).toList
  }

  /* P19 */
  def rotateTailRecursive(n:Int, ls:List[Any]):List[Any] = {
    val i = if (n>=0) n else ls.size + n
    def rotateRecursive(acc:Int, pos:Int, result:List[Any], tail:List[Any]):List[Any] = tail match {
        case x :: xs => {
            if (acc == pos) x:: xs ::: reverse(result)
            else rotateRecursive(acc+1, pos, x :: result, xs)
        }
    }
    rotateRecursive(0, i, Nil, ls)
  }

  /* P20 */
  def removeAtTailRecursive(n:Int, ls:List[Any]):Tuple2[List[Any],Any] = {
    def removeAtRecursive(acc:Int, n:Int, ls:List[Any], result:List[Any]):List[Any] = (acc equals n, ls) match {
        case (true, x::xs) => removeAtRecursive(acc+1, n, xs, result)
        case (false, x::xs) => removeAtRecursive(acc+1, n, xs, x::result)
        case (_, Nil) => reverse(result)
    }
    Tuple2(removeAtRecursive(0, n, ls, Nil), ls(n))
  }

  /* P21 */
  def insertAtTailRecursive(item:Any, pos:Int, ls:List[Any]):List[Any] = {
    def insertAtRecursive(acc:Int, pos:Int, ls:List[Any], result:List[Any]):List[Any] = (acc equals pos, ls) match {
        case (true, x::xs) => insertAtRecursive(acc+1, pos, xs, x::item::result)
        case (false, x::xs) => insertAtRecursive(acc+1, pos, xs, x::result)
        case (_, Nil) => reverse(result)
    }
    insertAtRecursive(0, pos, ls, Nil)
  }

  /* P22 */ 
  def rangeTailRecursive(s:Int, e:Int):List[Int] = {
    def rangeRecursive(acc:Int, s:Int, e:Int, result:List[Int]):List[Int] =  {
        if (acc < s) rangeRecursive(acc+1, s, e, result)
        else if (acc == e) reverse(acc :: result)
        else rangeRecursive(acc+1, s, e, acc :: result)
    }
    rangeRecursive(s, s, e, Nil)
  }
}
