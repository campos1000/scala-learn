package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    b <- arbitrary[Boolean]
    h <- oneOf(const(this.empty), genHeap)
  } yield if (b) {
    insert(k ,h)
  } else h

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>
    val m = arbitrary[Int].sample.get
    val n = arbitrary[Int].sample.get

    val min = if (m < n) m else n
    if (isEmpty(h)) {
      val h1 = insert(m, h)
      val h2 = insert(n, h1)
      findMin(h2) == min
    } else {
      val curMin = findMin(h);
      val h1 = insert(m, empty)
      val h2 = insert(n, h1)
      val newHah = meld(h2, h)
      val newAssMin = if (curMin < min) curMin else min
      findMin(newHah) == newAssMin
    }
  }

  property("gen3") = forAll { (h: H) =>
    val m = arbitrary[Int].sample.get
    isEmpty(deleteMin(insert(m, empty)))
  }

  property("gen4") = forAll { (h: H) =>
    val list = listOfN[Int](5, arbitrary[Int]).sample.get.sorted
    val newH = addList(list, empty)
    checkList(list, newH)
  }

  def checkList(list: List[Int], h: H) : Boolean = {
    if (list.isEmpty) true
    else (list.head == findMin(h)) && checkList(list.tail, deleteMin(h))
  }

  def addList(list: List[Int], h: H) : H = {
    if (list.isEmpty) h
    else addList(list.tail, insert(list.head, h))
  }
}
