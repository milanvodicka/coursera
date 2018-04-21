def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => (y * y) :: squareList1(ys)
  }

def squareList(xs: List[Int]): List[Int] =
  xs map (x => x * x)

squareList(List(1,2,3,4))

val numbers = List(-1, 2, 4, -5, 10, 11, -22)

val fruit = List(
  "Apple",
  "Pineapple",
  "Banana",
  "Blueberry",
  "Strawberry",
  "Orange",
)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case y :: ys =>
    val (first, second) = xs.span(_ == y)
    first :: pack(second)
}

pack(List("a", "a", "a", "b", "c", "c", "a"))

pack(List("a", "a", "a", "b", "c", "c", "a")) ==
List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

encode(List("a", "a", "a", "b", "c", "c", "a")) ==
List(("a", 3), ("b", 1), ("c", 2), ("a", 1))