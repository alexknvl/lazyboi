package lazyboi

object syntax {
  implicit class force[A](val a: A) extends AnyVal {
    def force(implicit ev: NFData[A]): Need[A] = ev.force(a)
  }

  implicit class whnf[A](val a: A) extends AnyVal {
    def whnf(implicit ev: WHNFData[A]): Need[A] = ev.force(a)
  }

  object yolo {
    implicit class head[A](val list: LazyList[A]) extends AnyVal {
      def head(implicit A: Delay[A]): A = A.delay(list.value.map { case LazyList.Cons(a, _) => a })
    }
    implicit class unSome[A](val v: Maybe[A]) extends AnyVal {
      def unSome(implicit A: Delay[A]): A = A.delay(v.value.map { case Maybe.Some(a) => a })
    }
  }

  object wololo {
    implicit class head[A](val list: LazyList[A]) extends AnyVal {
      def head: A = list.value.value match { case LazyList.Cons(a, _) => a }
    }
    implicit class unSome[A](val v: Maybe[A]) extends AnyVal {
      def unSome: A = v.value.value match { case Maybe.Some(a) => a }
    }
  }
}
