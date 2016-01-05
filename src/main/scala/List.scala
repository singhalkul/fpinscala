
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

  def length: Int = this.foldRight(0)((_, b) => 1 + b)

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    def loop(list: List[A], z: B, res: B)(f: (B, A) => B): B = {
      list match {
        case Nil => res
        case Cons(h, t) => loop(t, z, f(res, h))(f)
      }
    }
    loop(this, z, z)(f)
  }

  def reverseFold = this.foldLeft(Nil:List[A])((b, a) => Cons(a, b))
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
}