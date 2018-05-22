import scala.util.DynamicVariable

class StackableVariables[T](init: T) {
  private var values: List[T] = List(init)
  def value = values.head
  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values
    try op finally values = values.tail
  }
}

class Signal[T](expr: => T) {
  import Signal._ // because of caller

  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()

  update(expr)

  protected def update(expr: => T) = {
    myExpr = () => expr
    computeValue()
  }

  protected def computeValue(): Unit = {
    // current signal is the caller
    val newValue = caller.withValue(this)(myExpr())
    if (newValue != myValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }

  def apply(): T = {
    // this caller.value is set in computeValue by means of withValue
    observers += caller.value
    // isn't observers the private val ?
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }
}

object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}

object Signal {
  // this caller is same (global) for all signals!
  // it is like static variable
//  private val caller = new StackableVariables[Signal[_]](NoSignal)
  // todo: rewrite with implicit (pass caller to the expression)
  private val caller = new DynamicVariable[Signal[_]](NoSignal)
  def apply[T](expr: => T) = new Signal(expr)
}

class Var[T](expr: => T) extends Signal[T](expr) {
  // makes it accessible from the outside
  override def update(expr: => T) = super.update(expr)
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}
