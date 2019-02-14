package lazyboi

import cats.{Order, Show}
import cats.syntax.show._

sealed trait PairingHeap[+A] {
  import PairingHeap._

  def value: Need[Strict[A]]
  def isEmpty: Boolean = value.value match {
    case Empty => true
    case Node(_, _, _) => false
  }

  def insert[AA >: A : Order](v: AA): PairingHeap[AA] =
    PairingHeap.merge(this, Node(v, Empty, Empty))

  def findMin: Option[A] = value.value match {
    case Empty => None
    case Node(x, _, _) => Some(x)
  }

  def deleteMin[AA >: A : Order]: Option[(AA, PairingHeap[AA])] = value.value match {
    case Empty => None
    case Node(x, a, b) => Some((x, (a : PairingHeap[AA]) merge b))
  }

  def merge[AA >: A : Order](that: PairingHeap[AA]): PairingHeap[AA] =
    PairingHeap.merge(this, that)

  def toLazyList: LazyList[A] = LazyList.Lazy(value.map {
    case Empty => LazyList.Nil
    case Node(x, a, b) => x +: (a.toLazyList ++ b.toLazyList)
  })
}
object PairingHeap {
  final case class Lazy[A](value: Need[Strict[A]]) extends PairingHeap[A]
  sealed trait Strict[+A] extends PairingHeap[A]
  final case object Empty extends Strict[Nothing] {
    val value: Need[Empty.type] = Need.now(this)
  }
  final case class Node[+A](current: A, left: PairingHeap[A], right: PairingHeap[A]) extends Strict[A] {
    def value: Need[Node[A]] = Need.now(this)
  }

  def empty[A]: PairingHeap[A] = Empty

  def merge[A](a: PairingHeap[A], b: PairingHeap[A])(implicit A: Order[A]): PairingHeap[A] =
    Lazy(a.value.flatMap {
      case Empty => b.value
      case a@Node(x, _, _) => b.value.flatMap {
        case Empty => Need.now(a)
        case b@Node(y, _, _) =>
          if (A.lt(x, y)) link(a, b)
          else link(b, a)
      }
    })

  def link[A: Order](h1: Node[A], h2: Node[A]): Need[Strict[A]] =
    h1.left.value.map {
      case Empty => Node(h1.current, h2, h1.right)
      case a: Node[A] => Node(h1.current, Empty, merge(merge(h2, a), h1.right))
    }

  implicit def delay[A]: Delay[PairingHeap[A]] =
    (a: Need[PairingHeap[A]]) => Lazy(a.flatMap(_.value))

  implicit def show[A: Show]: Show[PairingHeap[A]] =
    (t: PairingHeap[A]) => t.value.value match {
      case Empty => "Empty"
      case Node(x, a, b) => show"Node($x, $a, $b)"
    }
}