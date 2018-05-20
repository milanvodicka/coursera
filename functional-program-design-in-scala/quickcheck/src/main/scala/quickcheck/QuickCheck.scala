package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def insert(is: List[Int], h: H): H = is.foldLeft(h)((newH, nextI) => insert(nextI, newH))

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      heap <- genHeap
    } yield insert(x, heap)
  )

  lazy val genNonEmptyHeap: Gen[H] = for {
    is <- arbitrary[List[Int]] if is.nonEmpty
    heap = insert(is, empty)
  } yield heap

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def equals(h1: H, h2: H): Boolean = (h1, h2) match {
    case _ if isEmpty(h1) && isEmpty(h2) => true
    case _ if !isEmpty(h1) && !isEmpty(h2) =>
      findMin(h1) == findMin(h2) && equals(deleteMin(h1), deleteMin(h2))
    case _ => false
  }

  def holdsOrdering(h:H, min: Option[Int] = None): Boolean = (h, min) match {
    case _ if isEmpty(h) => true
    case (_, None) => holdsOrdering(deleteMin(h), Some(findMin(h)))
    case (_, Some(m)) =>
      m <= findMin(h) && holdsOrdering(deleteMin(h), Some(findMin(h)))
  }

  property("isEmpty is consistent with findMin") = forAll { h: H =>
    try {
      findMin(h)
      !isEmpty(h)
    } catch {
      case _: NoSuchElementException => isEmpty(h)
    }
  }

  property("subsequent insert and delete upon empty heap creates an empty heap") = forAll { (h:H, i:Int) =>
    isEmpty(h) ==> isEmpty(deleteMin(insert(i, h)))
  }

  property("delete from non-empty heap creates new heap") = forAll { h:H =>
    !isEmpty(h) ==> !equals(h, deleteMin(h))
  }

  // bogus 5 failed at this
  property("inserting new value creates new heap") = forAll { (h: H, i: Int) => !equals(h, insert(i,h)) }

  // bogus 1, 2 and 3 failed at this
  property("findMin works after inserting new value into heap") = forAll { (h: H, i: Int) =>
    (h, i) match {
      case _ if isEmpty(h) =>
        val lessI = if (i == Int.MinValue) i else i - 1
        findMin(insert(List(i, lessI), h)) == lessI &&
        findMin(deleteMin(insert(List(i, lessI), h))) == i &&
        findMin(insert(i, h)) == i
      case _ if findMin(h) == i => findMin(insert(i, h)) == i
      case _ if findMin(h) > i => findMin(insert(i, h)) == i
      case _ if findMin(h) < i => findMin(insert(i, h)) < i
    }
  }

  property("melding is commutative") = forAll { (h1: H, h2: H) =>
    equals(meld(h1, h2), meld(h2, h1))
  }

  property("melding is associative") = forAll { (h1: H, h2: H, h3: H) =>
    equals(meld(h1, meld(h2, h3)), meld(meld(h1, h2), h3))
  }

  property("modifying does not screw the ordering") = forAll { (h: H, i: Int) =>
    if (!isEmpty(h)) holdsOrdering(deleteMin(h)) && holdsOrdering(insert(i, h))
    else holdsOrdering(insert(i, h))
  }

  property("melding creates new heaps with the smallest value across both source heaps as new min") =
    forAll(genNonEmptyHeap, genNonEmptyHeap) { (h1: H, h2: H) =>
      val h3 = meld(h1, h2)
      findMin(h3) == Math.min(findMin(h1), findMin(h2))
    }

  property(
    "melding non-empty with empty heap creates the same heap"
  ) = forAll(genNonEmptyHeap) { h1: H =>
    equals(h1, meld(h1, empty))
  }

}
