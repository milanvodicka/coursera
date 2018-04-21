val n = 7

(1 until n)
  .flatMap(i => (1 until i).map(j => (i, j)))
  .filter(_._1 == 5)

case class Person(name: String, age: Int)

val persons =
  Person("Milan", 28) ::
  Person("Lenka", 29) ::
  Person("Emily", 1) ::
  Person("Haskell", 0) ::
  Nil

for {
  p <- persons if p.age > 20
} yield p.name

for {
  i <- 1 until n
  j <- 1 until i
  if i == 5
} yield (i, j)

def scalarProduct(xs: List[Int], ys: List[Int]): Double =
  (for ((x, y) <- xs zip ys) yield x * y).sum