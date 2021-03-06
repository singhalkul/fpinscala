
sealed trait List[+A] {
  def tail: List[A] = ???

  def drop(n: Int): List[A] = ???

  def dropWhile(f: A => Boolean): List[A] = this

  def setHead[B >: A](x: B): List[B] = ???

  def init: List[A] = {
    this match {
      case Nil => ???
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, tail.init)
    }
  }

  def foldRight[B](z: B)(f: (A, B) => B): B = {
    this match {
      case Nil => z
      case Cons(h, t) => f(h, t.foldRight(z)(f))
    }
  }

  def lengthUsingFoldRight: Int = this.foldRight(0)((_, b) => 1 + b)

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    def loop(list: List[A], z: B, res: B)(f: (B, A) => B): B = {
      list match {
        case Nil => res
        case Cons(h, t) => loop(t, z, f(res, h))(f)
      }
    }
    loop(this, z, z)(f)
  }

  def reverseUsingFoldLeft = this.foldLeft(Nil: List[A])((b, a) => Cons(a, b))

  def foldLeftViaFoldRight[B](z: B)(f: (B, A) => B): B = {
    this.foldRight((b: B) => b)((a, g) => b => g(f(b, a)))(z)
  }

  def foldRightViaFoldLeft[B](z: B)(f: (A, B) => B): B = {
    this.foldLeft((b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }

  def appendUsingFoldRight[B >: A](list: List[B]): List[B] = this.foldRight(list)(Cons(_, _))

  def flattenUsingFold: List[A] = this.foldLeft(Nil: List[A])((b: List[A], a: A) => b.appendUsingFoldRight(a.asInstanceOf[List[A]]))

  def map[B](f: A => B): List[B] = this.foldRight(Nil: List[B])((h, t) => Cons(f(h), t))

  def filter(f: A => Boolean) = this.foldRight(Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[B](f: A => List[B]): List[B] = this.foldLeft(Nil: List[B])((t: List[B], h: A) => t.appendUsingFoldRight(f(h)))

  def filterUsingFlatMap(f: A => Boolean) = this.flatMap(a => if (f(a)) List(a) else Nil)
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, override val tail: List[A]) extends List[A] {
  override def drop(n: Int): List[A] = {
    def loop(n: Int, list: List[A]): List[A] = {
      if (n == 0) list else loop(n - 1, list.tail)
    }
    loop(n, this)
  }

  override def dropWhile(f: A => Boolean): List[A] = if (f(head)) tail.dropWhile(f) else this

  override def setHead[B >: A](x: B): List[B] = Cons(x, tail)
}

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def addPairs[A](list1: List[A], list2: List[A], f: (A, A) => A): List[A] = {
    (list1, list2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), addPairs(t1, t2, f))
    }
  }

}