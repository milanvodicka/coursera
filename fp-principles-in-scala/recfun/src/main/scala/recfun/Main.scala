package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("should be true")
    println(balance("()()()()".toList))
    println(balance("((d)(dd(dddd)d)d)".toList))
    println(balance("".toList))
    println("should be false")
    println(balance("())(()".toList))
    println(balance("(".toList))
    println(balance(")".toList))
    println(balance("(()".toList))
    println(balance("())".toList))

    println(countChange(4, List(1,2)))
    println(countChange(6, List(1,2,5)))
    println(countChange(-1, List(1,2,5)))
    println(countChange(0, List(1,2,5)))
    println(countChange(0, List()))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = (c, r) match {
      case (0, _) => 1
      case _ if c == r => 1
      case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def findLeft(chars: List[Char]): Boolean = chars match {
        case Nil => true
        case ')' :: xs => false
        case '(' :: xs => findRight(xs)
        case _ :: xs => findLeft(xs)
      }

      def findRight(chars: List[Char], acc: Int = 1): Boolean = chars match {
        case Nil => false
        case ')' :: xs => if (acc - 1 > 0) findRight(xs, acc - 1) else findLeft(xs)
        case '(' :: xs => findRight(xs, acc + 1)
        case _ :: xs => findRight(xs, acc)
      }

      findLeft(chars)
    }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
      case (_, Nil) => 0
      case _ if money < 0 => 0
      case (0, _) => 1
      case (_, x :: xs) => countChange(money - x, coins) + countChange(money, xs)
    }
  }
