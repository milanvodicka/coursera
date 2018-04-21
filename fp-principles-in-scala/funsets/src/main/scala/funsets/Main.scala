package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  printSet(singletonSet(28))
  printSet(union(singletonSet(28), singletonSet(29)))
  printSet(intersect(
    union(
      singletonSet(1),
      singletonSet(2)
    ),
    union(
      singletonSet(2),
      singletonSet(3)
    )
  ))
  printSet(diff(
    union(
      singletonSet(1),
      singletonSet(2)
    ),
    union(
      singletonSet(2),
      singletonSet(3)
    )
  ))
  printSet(filter(union(
    singletonSet(1),
    singletonSet(2)
  ), _ % 2 == 0))
  println(forall(singletonSet(2), _ == 2))
  println(forall(singletonSet(2), _ == 3))
  println(exists(singletonSet(2), _ == 2))
  println(exists(singletonSet(2), _ == 3))
  println(contains(map(singletonSet(5), _*2), 10))
  println(contains(map(singletonSet(5), _*2), 5))
}