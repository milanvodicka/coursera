import scala.reflect.ClassTag
import scala.util.Random

trait Generator[+T] {
  self => // an alias for "this"

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }

  // possibly dangerous
  def withFilter(f: T => Boolean): Generator[T] = new Generator[T] {
    override def generate: T = {
      val randVal = self.generate
      if (f(randVal)) randVal
      else generate
    }
  }
}


val integers = new Generator[Int] {
  def generate = Random.nextInt()
}

val booleans = for (x <- integers) yield x > 0

val pairs = for {
  x <- integers if x > 0
  y <- integers if y < 0
} yield (x, y)

// generator toolkit

def single[T](x: T) = new Generator[T] {
  def generate = x
}

def choose(lo: Int, hi: Int) =
  for (x <- integers) yield lo + Math.abs(x % (hi - lo))

def oneOf[T](xs: Generator[T]*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx).generate

// how to?
//def oneOf[T](xs: T*): Generator[T] =
//  for (idx <- choose(0, xs.length)) yield xs(idx)

// lists generator

def lists: Generator[List[Int]] = oneOf(
  single(Nil),
  for {
    head <- integers
    tail <- lists
  } yield head :: tail
)

// trees generator

trait Tree[T]

case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]

case class Leaf[T](x: T) extends Tree[T]

// is this correct ?
def trees: Generator[Tree[Int]] = oneOf(
  for (x <- integers) yield Leaf(x),
  for {
    l <- trees
    r <- trees
  } yield Node(l, r)
)