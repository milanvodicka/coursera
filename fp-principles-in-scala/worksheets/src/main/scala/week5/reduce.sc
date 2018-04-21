def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((y, ys) => f(y) :: ys)

mapFun[Int, Int](List(1,2,3), _ * 2)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, length) => length + 1)

lengthFun(List(1,2,3,4))

def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys)(_ :: _)