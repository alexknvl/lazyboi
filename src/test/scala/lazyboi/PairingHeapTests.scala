package lazyboi

import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import cats.instances.int._

import scala.annotation.tailrec
import scala.collection.mutable

class PairingHeapTests extends PropSpec with GeneratorDrivenPropertyChecks {
  def unfold[S, A](s: S)(f: S => Option[(A, S)]): List[A] = {
    @tailrec def go(s: S, builder: mutable.Builder[A, List[A]]): List[A] =
      f(s) match {
        case None => builder.result()
        case Some((a, n)) => go(n, builder += a)
      }

    go(s, List.newBuilder[A])
  }

  property("pairing heap sorts") {
    forAll { list: List[Int] =>
      val heap = list.foldLeft(PairingHeap.empty[Int])((acc, x) => acc.insert(x))

      val sorted1 = unfold(heap)(_.deleteMin)
      val sorted2 = list.sorted
      assert(sorted1 === sorted2)
    }
  }
}
