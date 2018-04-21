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

object List {
  // List(1,2) = List.apply(1,2)
  def apply[T](a: T, b: T): List[T] =
    new Cons(a, new Cons(b, new Nil))
  def apply() = new Nil
}