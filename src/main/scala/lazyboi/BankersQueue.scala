package lazyboi

final case class BankersQueue[A](ls: Int, left: LazyList[A],
                                 rs: Int, right: LazyList[A]) {
  import LazyList.{ Nil, Cons }
  import BankersQueue.check

  def isEmpty: Boolean = ls == 0

  def snoc[AA >: A](x: AA): BankersQueue[AA] =
    check(ls, left, rs + 1, Cons(x, right))

  def snocReversed[AA >: A](xs: LazyList[AA]): BankersQueue[AA] =
    check(ls, left, rs + 1, xs ++ right)

  def uncons: Option[(A, BankersQueue[A])] = {
    left.value.value match {
      case Nil => None
      case Cons(a, as) => Some((a, check(ls - 1, as, rs, right)))
    }
  }
}
object BankersQueue {
  import LazyList.{ Nil, Cons }

  def ofLazyList[A](list: LazyList[A]): BankersQueue[A] =
    BankersQueue(list.size, list, 0, LazyList.Nil)

  def empty[A] = BankersQueue(0, LazyList.Nil, 0, LazyList.Nil)

  private def check[A]
  (ls: Int, left: LazyList[A],
   rs: Int, right: LazyList[A]
  ): BankersQueue[A] = {
    if (rs <= ls) BankersQueue(ls, left, rs, right)
    else BankersQueue(ls + rs, left ++ right.reverse, 0, Nil)
  }
}