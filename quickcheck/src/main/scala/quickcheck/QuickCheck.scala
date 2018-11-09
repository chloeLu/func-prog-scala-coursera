package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = Gen.frequency(
    (1, const(empty)),
    (5, for {
      newval <- Gen.choose(-10000,10000)
      h <- Gen.frequency((1, const(empty)), (5, genHeap))
    } yield insert(newval, h))
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  //natural number heap; returns 0 if heap is empty
  def findMinHelper(h: H): A = {
    if (isEmpty(h)) -10001 else findMin(h)
  }

  property("gen1") = forAll { h: H =>
    val m = findMinHelper(h)
    findMin(insert(m, h)) == m
  }

  property("insert") = forAll { h: H =>
    val m = findMinHelper(h)
    val newval = m+1
    val expectedval = if(isEmpty(h)) newval else m
    findMin(insert(newval, h)) == expectedval
  }

  property("insert num < heap min") = forAll { h: H =>
    val m = findMinHelper(h)
    val newval = m-1
    val h1 = insert(newval, h)
    findMin(h1) == newval
  }

  property("insert num < heap min then deleteMin") = forAll { h: H =>
    val m = findMinHelper(h)
    val newval = m-1
    val h1 = deleteMin(insert(newval, h))
    val m1 = findMinHelper(h1)
    m1 == m
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val m0: Int = {
      if (isEmpty(h1) && isEmpty(h2)) -10001
      else if (isEmpty(h1)) findMin(h2)
      else if (isEmpty(h2)) findMin(h1)
      else Math.min(findMin(h1), findMin(h2))
    }
    val meldedHeap: H = meld(h1, h2)
    val m: Int = findMinHelper(meldedHeap)
    m0 == m
  }

  property("deleteMin") = forAll { h: H =>
    val m = findMinHelper(h)
    val h1 = deleteMin(insert(m, h))
    findMinHelper(h1) == m
  }

  property("deleteMin2") = forAll { h: H =>
    if (!isEmpty(h)){
      val m = findMin(h)
      val h1 = deleteMin(h)
      isEmpty(h1) || findMin(h1) > m
    } else {
      true
    }
  }

}
