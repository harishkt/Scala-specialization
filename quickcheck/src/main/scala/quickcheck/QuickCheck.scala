package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(genHeap,Gen.const(empty))
  } yield insert(x,h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("Checking for minimum element") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Smallest of two elements") = forAll { (x:A,y:A) =>
    val h  = insert(x,insert(y,empty))
    findMin(h) == min(x,y)
  }

  property("Minimum of two heaps should be same as the minimum of the melded heap")  = forAll { (h1:H, h2: H) =>
    findMin(meld(h1,h2)) == min(findMin(h1),findMin(h2))
  }

  property("hint3") = forAll { (h1:H) =>
    def isSorted(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val min_val = findMin(h)
        val new_heap = deleteMin(h)
        isEmpty(new_heap) || ((min_val <= findMin(new_heap)) && isSorted(new_heap))

      }
    }
    isSorted(h1)
  }

  property("melding heaps and finding the minimum") = forAll { (h:H, i:H) =>
    val minH = findMin(h)
    val minI = findMin(i)
    val merged = meld(h, i)
    findMin(merged) == min(minH,minI)
  }

  property("Two heaps should be equal if recursively removing min elements result in same elements until empty") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("The minimal value of 2 heaps should be the minimal after displacing it from heap 1 to 2 and melding both") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = min(m1,m2)
    findMin(meld(deleteMin(h1), insert(m, h2))) == m
  }




}
