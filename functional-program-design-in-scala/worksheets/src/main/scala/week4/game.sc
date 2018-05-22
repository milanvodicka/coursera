import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import scala.util.{Failure, Success}

/*
type Coin
type Treasure

trait Adventure {
  def collectCoins(): List[Coin]
  def buyTreasure(coins: List[Coin]): Treasure
}

val adventure = Adventure()
val coins = adventure.collectCoins()
val treasure = adventure.buyTreasure(coins)
*/

// then to socket reading from memory and sending packets to Europe

// Future
// execution context?


var names = List("Milan", "Lenka", "Emily")

def name(): Future[String] = Future {
  println("Evaluating a future!")
  val name = names.head
  names = names.tail
  name
}

val fut = name()

fut onComplete {
  case Success(name) => println(name)
}

fut onComplete {
  case Success(name) => println(name)
}

fut onComplete {
  case Success(name) => println(name)
}

Await.ready(fut, 10 seconds)

// I expected three different values

// recover -> map
// recoverWith -> flatMap
// but for the error cases

// fallbackTo

def fallbackTo[T](a: => Future[T], b: => Future[T]): Future[T] = {
  a recoverWith {
    case _ => b recoverWith {
      case _ => a
    }
  }
}

// try a, if fails try b, if that fails too, return a so we get first error

// awaitable

// postfix operations

import scala.language.postfixOps

// apply with two arguments catches this: 123 seconds

def retry[T](times: Int)(exp: => Future[T]): Future[T] = {
  if (times <= 0) {
    Future.failed(new Exception)
  } else {
    fallbackTo(exp, retry(times - 1)(exp))
  }
}

// recursion is simpler
def retryFoldLeft[T](times: Int)(exp: => Future[T]): Future[T] = {
  val es = (1 to times).map(_ => () => exp)
  es.foldLeft[Future[T]](Future.failed(new Exception)) { (f, e) =>
    f recoverWith { case _ => e() }
  }
}
