trait Generator[+T] {
  self => // alias for this

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]) = new Generator[S] {
    override def generate = f(self.generate).generate
  }
}

def single[T](x: T): Generator[T] = new Generator[T] {
  override def generate = x
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  override def generate: Int = rand.nextInt
}

def choose(low: Int, high: Int): Generator[Int] =
  for (x <- integers; foo = println(x)) yield low + Math.abs(x % (high - low))

def oneOf[T](xs: T*): Generator[T] =
  for (idx <- choose(0, xs.length)) yield xs(idx)

val booleans = for (x <- integers) yield x > 0

val pairs = for(x <- integers; y <- integers) yield (x, y)

def lists: Generator[List[Int]] = {
  def emptyList = single(Nil)

  def nonEmptyList = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyList else nonEmptyList
  } yield list
}

// todo tree generator
// todo revise how for expressions are translated