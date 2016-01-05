sealed trait List[+A] {
  def tail: List[A] = ???

  def drop(n: Int): List[A] = ???

  def dropWhile(f: A => Boolean): List[A] = this
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, override val tail: List[A]) extends List[A] {
  override def drop(n: Int): List[A] = {
    def loop(n: Int, list: List[A]): List[A] = {
      if (n == 0) list else loop(n - 1, list.tail)
    }
    loop(n, this)
  }

  override def dropWhile(f: A => Boolean): List[A] = {
    if (f(head)) tail.dropWhile(f)
    else this
  }
}

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}