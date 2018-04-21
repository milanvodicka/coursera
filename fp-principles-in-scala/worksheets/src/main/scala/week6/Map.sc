val romanNumbers = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)
val capitalOfCountry = Map("US" -> "Washington", "Slovakia" -> "Bratislava", "Czech Republic" -> "Prague")

// map are also functions Key -> Value

capitalOfCountry("Slovakia")
capitalOfCountry get "Germany"
capitalOfCountry get "US"

def showCapital(country: String) = (capitalOfCountry get country) match {
  case None => "nothing has been found"
  case Some(capital) => capital
}

showCapital("Slovakia")
showCapital("Australia")

// sortWith, sorted

val fruits = List("apple", "pineapple", "banana", "blueberry", "kiwi")

fruits.groupBy(_.head)

// polynomials
// map from exponents -> coefficients
// x3 - 2x + 5
// Map(0 -> 5, 1 -> -2, 3 -> 1)

class Poly(val terms0: Map[Int, Double]) {

  // this somehow overrides the default constructor
  // but it calls default constructor right after
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0.withDefaultValue(0.0)

  def + (other: Poly) = new Poly(other.terms.foldLeft(terms)(addTerm))

  def addTerm(acc: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }

  override def toString: String =
    (for ((exp, coef) <- terms.toList.sorted.reverse) yield coef + "x^" + exp) mkString " + "
}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(0 -> 3.0, 3 -> 7.0)

p1 + p2
