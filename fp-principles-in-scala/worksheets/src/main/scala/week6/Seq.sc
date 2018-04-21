// +:
// :+

//       Iterable
//    |     |     |
//    Seq  Set   Map
//    | |
// List Vector

// Arrays Strings <- Java Universe

val xs = Array(1,2,3,4)

xs map (_*2)

val s = "Milan"

s filter (_.isLower)

// Range

val a: Range = 1 to 5
val b: Range = 1 until 5

(1 to 9 by 3 toList)
(6 to 1 by -2 toList)

// Seqs
/*
  exists
  forall
  zip
  unzip
  flatMap
  sum
  product
  max
  min
*/

def combOfNumbers(m: Int, n: Int): IndexedSeq[(Int, Int)] =
  (1 to m) flatMap (x => (1 to n) map (y => (x, y)))

combOfNumbers(3,4)

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys) map { case (x, y) => x * y } sum

scalarProduct(Vector(1,2,3), Vector(4,5,6))

def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)

isPrime(8)
isPrime(7)







