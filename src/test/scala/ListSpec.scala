import org.specs2.mutable.Specification

class ListSpec extends Specification {

  "1 list object" should {
    "create empty list" in {
      List() mustEqual Nil
    }

    "create list of size 1" in {
      List(1) mustEqual Cons(1, Nil)
    }

    "create list of size > 1" in {
      List(1, 2, 3) mustEqual Cons(1, Cons(2, Cons(3, Nil)))
    }
  }

  "2 tail" should {
    "remove first element" in {
      List(1, 2, 3).tail mustEqual Cons(2, Cons(3, Nil))
    }
  }

  "3 drop" should {
    "remove 1 element from the list of 1" in {
      List(1).drop(1) mustEqual Nil
    }

    "remove 2 element from the list of 3" in {
      List(1, 2, 3).drop(2) mustEqual Cons(3, Nil)
    }

  }

  "4 drop while" should {

    "drop till 1 is reached" in {
      List(1, 2, 3).dropWhile(_ == 1) mustEqual Cons(2, Cons(3, Nil))
    }

    "drop nothing if condition is false" in {
      List(1, 2, 3).dropWhile(_ => false) mustEqual Cons(1, Cons(2, Cons(3, Nil)))
    }

    "drop everything if condition is true" in {
      List(1, 2, 3).dropWhile(_ => true) mustEqual Nil
    }

  }

  "5 set head" should {
    "replace head in the list" in {
      List(1, 2).setHead(3) mustEqual Cons(3, Cons(2, Nil))
    }
  }

  "6 init" should {

    "return Nil for list of size 1" in {
      List(1).init mustEqual Nil
    }

    "return first 2 elements for list of size 3" in {
      List(1, 2, 3).init mustEqual Cons(1, Cons(2, Nil))
    }
  }

  "fold right" should {

    "sum elements of list" in {
      List(1, 2, 3).foldRight(0)(_ + _) mustEqual 6
    }

    "multiply elements of list" in {
      List(1, 2, 3).foldRight(1)(_ * _) mustEqual 6
    }

    "8 create list" in {
      List(1, 2, 3).foldRight(Nil: List[Int])(Cons(_, _)) mustEqual Cons(1, Cons(2, Cons(3, Nil)))
    }
  }

  "9 .length" should {

    "return 3 for list of 3 elements" in {
      List('a', 'b', 'c').length mustEqual 3
    }

    "return 0 for empty list" in {
      Nil.length mustEqual 0
    }
  }

  "10 fold left" should {
    "11 sum elements of list" in {
      List(1, 2, 3).foldLeft(0)(_ + _) mustEqual 6
    }

    "11 multiply elements of list" in {
      List(1, 2, 3).foldLeft(1)(_ * _) mustEqual 6
    }

    "11 calculate length of list" in {
      List(8, 9, 11, 12).foldLeft(0)((b, a) => 1 + b) mustEqual 4
    }
  }

  "12 reverse fold" should {
    "reverse a list" in {
      List(1,2,3).reverseFold mustEqual Cons(3, Cons(2, Cons(1, Nil)))
    }
  }

}