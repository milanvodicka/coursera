def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

product(x => x)(1,5)

def factorial = (x: Int) => product(y => y)(1, x)

factorial(4)

def general(unit: Int, combine: (Int, Int) => Int)
           (f: Int => Int)
           (a: Int, b: Int): Int =
              if (a > b) unit
              else combine(
                f(a),
                general(unit, combine)(f)(a + 1, b)
              )


class Rational(x: Int, y: Int) {

  // alternative construction
  def this(x: Int) = this(x, 1)

  require(y != 0, "denominator must be nonzero")

  // greatest common divisor
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)

  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)

  def neg =
    new Rational(-numer, denom)

  def unary_- = neg

  def add(r: Rational) =
    new Rational(
      numer * r.denom + r.numer * denom,
      denom * r.denom
    )

  def +(r: Rational) = add(r)

  def sub(r: Rational) = this + -r

  def -(r: Rational) = sub(r)

  def less(r: Rational) =
    numer * r.denom < r.numer * denom

  def max(r: Rational) =
    if (less(r)) r else this

  override def toString = numer + "/" + denom
}

//val weird = new Rational(3, 0)
//weird + weird

val x = new Rational(3,4)
val y = new Rational(2,3)

x.add(y)

x add y

x + y

val a = new Rational(1,3)
val b = new Rational(5,7)
val c = new Rational(3,2)

a.sub(b).sub(c)

a sub b sub c

a - b - c

a + b - c




