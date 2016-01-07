sealed trait Tree[+A] {

  def size: Int = {
    this match {
      case Leaf(_) => 1
      case Branch(l, r) => l.size + r.size + 1
    }
  }
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(a) => a
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](tree: Tree[A])(l: A => B)(b: (B,B) => B): B = {
    tree match {
      case Leaf(a) => l(a)
      case Branch(left,right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
  }
}