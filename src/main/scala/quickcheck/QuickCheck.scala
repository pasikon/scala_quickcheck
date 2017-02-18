package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  private val genNonEmpty: Gen[H] = for {
    i: Int <- arbitrary[Int]
    j <- frequency((1, const(empty)), (100, genHeap))
  } yield insert(i, j)

  private val genEmpty = const(empty)

  lazy val genHeap: Gen[H] = frequency(10 -> genNonEmpty, 1 -> genEmpty)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  implicit val rndInt1 = arbitrary[Int]
  implicit val rndInt2 = arbitrary[Int]
  implicit val heapContainer2: Arbitrary[List[H]] = Arbitrary(Gen.listOfN[H](2, genNonEmpty))

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //wiecej niz jedna wart min
  //  property("delete min") = forAll { (h: H) =>
  //    val m = if (isEmpty(h)) 0 else findMin(h)
  //    val dmin = if (isEmpty(h)) h else deleteMin(h)
  //    if (isEmpty(dmin)) true
  //      else findMin(dmin) != m
  //  }

  property("sequence") = forAll { (h: H) =>
    if (isEmpty(h)) true
    else {

      def delMi(he: H, acc: List[A]): List[A] = {
        if (isEmpty(he)) acc
        else
          delMi(deleteMin(he), acc :+ findMin(he))
      }
      val listSort: List[Int] = delMi(h, Nil)
      val listSortProper: List[Int] = listSort.sortBy(a => a)(ord)

      //      println(listSort)

      listSort == listSortProper
    }
  }

  property("build heap from obtained sorted list") = forAll { (h: H) =>
    if (isEmpty(h)) true
    else {

      def delMi(he: H, acc: List[A]): List[A] = {
        if (isEmpty(he)) acc
        else
          delMi(deleteMin(he), acc :+ findMin(he))
      }
      val listSort: List[Int] = delMi(h, Nil)
      val listSortProper: List[Int] = listSort.sortBy(a => a)(ord)

      def addMi(he: H, toAdd: List[A]): H = if (toAdd.nonEmpty) addMi(insert(toAdd.head, he), toAdd.tail) else he

      val builded: H = addMi(empty, listSort)
      delMi(builded, Nil) == listSortProper
    }
  }

  property("ins2val") = forAll { (h: H, i1: Int, i2: Int) =>
    if (!isEmpty(h)) true
    else {
      val min: Int = findMin(insert(i1, insert(i2, h)))
      val value: Boolean = if (i1 < i2) min == i1
      else if (i1 > i2) min == i2
      else min == i1
      value
    }
  }

  property("ins2val_rem") = forAll { (h: H, i1: Int) =>
    if (!isEmpty(h)) true
    else {
      val insert1 = insert(i1, h)
      val deleteMin1 = deleteMin(insert1)

      isEmpty(deleteMin1)
    }
  }

  property("melding 2 non empty & min") = forAll { (harr: List[H]) => {
    if (harr.size < 2) true
    else {
      //      println(harr)
      val h1: H = harr(0)
      val h2: H = harr(1)

      val min1: Int = findMin(h1)
      val min2: Int = findMin(h2)
      val melded: H = meld(h1, h2)

      val minM: Int = findMin(melded)

      val value: Boolean = if (min1 < min2) minM == min1
      else if (min1 > min2) minM == min2
      else minM == min1

      value
    }
  }

  }


}
