import math.Ordering

// merge sort
def mSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, Nil) => Nil
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val (fst, snd) = xs splitAt n
    merge(mSort(fst), mSort(snd))
  }
}


val numbers = List(-1, 2, 4, -5, 10, 11, -22)

val fruit = List(
  "Apple",
  "Pineapple",
  "Banana",
  "Blueberry",
  "Strawberry",
  "Orange",
)

mSort(numbers)
mSort(fruit)

//mSort(List(-1, 2, 4, -5, 10, 11, -22))((x, y) => x < y)
//mSort(fruit)((a, b) => a.length < b.length)
//mSort(fruit)((a, b) => a.compareTo(b) < 0)
//mSort(numbers)(Ordering.Int)
//mSort(fruit)(Ordering.String)
