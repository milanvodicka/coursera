trait Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Number(n: Int) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

case class Var(v: String) extends Expr

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Var(v) => v
  case Sum(a, b) => s"${show(a)} + ${show(b)}"
  case Prod(a: Sum, b:Sum) => s"(${show(a)}) * (${show(b)})"
  case Prod(a: Sum, b) => s"(${show(a)}) * ${show(b)}"
  case Prod(a, b: Sum) => s"${show(a)} * (${show(b)})"
  case Prod(a, b) => s"${show(a)} * ${show(b)}"
}

show(Sum(Prod(Var("a"), Var("b")), Var("c")))
show(Prod(Sum(Var("a"), Var("b")), Var("c")))
show(
  Sum(
    Sum(
      Number(5),
      Number(7)
    ),
    Sum(
      Number(14),
      Sum(
        Number(13),
        Number(10)
      )
    )
  )
)