val fruit = Set("apple", "banana", "pear")
val s = (1 to 6) toSet

// unordered
// no duplicates

// N - Queens

def nqueens(n: Int): Set[List[Int]] = {
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val rowColPairs = (0 until row).reverse.zip(queens)
    rowColPairs forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }
  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens

  placeQueens(n)
}

def showQueens(queens: List[Int]) = {
  var lines = for (col <- queens.reverse)
    yield List.fill(queens.length)("- ").updated(col, "x ").mkString
  "\n" + (lines mkString "\n")
}

(nqueens(9) map showQueens) mkString "\n"