trait Generator[T] {
  def generate: T
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  override def generate: Int = rand.nextInt
}

val booleans = new Generator[Boolean] {
  override def generate: Boolean = integers.generate > 0
}

val pairs = new Generator[(Int, Int)] {
  override def generate: (Int, Int) = (integers.generate, integers.generate)
}

