import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen._
import quickcheck.{BinomialHeap, IntHeap}

object QuickCheck extends IntHeap with BinomialHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      heap <- genHeap
    } yield insert(x, heap)
  )

  def printHeap(h: H): Unit = {
    if (isEmpty(h)) println("end\n\n");
    else {
      println(findMin(h))
      printHeap(deleteMin(h))
    }
  }

  def holdsOrdering(h:H, min: Option[Int] = None): Boolean = (h, min) match {
    case _ if isEmpty(h) => true
    case (_, None) => holdsOrdering(deleteMin(h), Some(findMin(h)))
    case (_, Some(m)) =>
      m <= findMin(h) && holdsOrdering(deleteMin(h), Some(findMin(h)))
  }
}

val samples = for {
  _ <- 1 to 1000000
  sample = QuickCheck.genHeap.sample
} yield sample

samples
  .collect({ case Some(m) => m })
  .count(h => !QuickCheck.holdsOrdering(h))