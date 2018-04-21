def last[T](xs: List[T]): T = xs match {
  case Nil => throw new Error("last of empty list")
  case x :: Nil => x
  case _ :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case Nil => throw new Error("init of empty list")
  case x :: Nil => Nil
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case Nil => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case _ if xs.length < n + 1 => xs
  case y :: ys if n == 0 => ys
  case y :: ys => y :: removeAt(n - 1, ys)
}

def removeAtSimple[T](n: Int, xs: List[T]): List[T] =
  xs.take(n) ::: xs.drop(n + 1) // why not ++

removeAtSimple(2, List('a', 'b', 'c', 'd'))

def flatten(xs: List[Any]): List[Any] = ???

