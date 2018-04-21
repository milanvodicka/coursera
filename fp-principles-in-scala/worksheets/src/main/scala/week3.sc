import week3._

val set1 = new NonEmpty(5, Empty, Empty) incl 10 incl 3 incl 4 incl 54
val set2 = Empty incl 1 incl 5 incl 33
val set3 = Empty incl 4 incl 7 incl 11
set2 union set3

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}

val list = new Cons[Int](10, new Cons[Int](20, new Cons[Int](30, new Nil[Int])))

def nth[T](n: Int, list: List[T]): T = {
  if (list.isEmpty) throw new IndexOutOfBoundsException()
  else if (n == 0) list.head
  else nth(n - 1, list.tail)
}

nth(0, list)
nth(1, list)
nth(2, list)
nth(3, list)d