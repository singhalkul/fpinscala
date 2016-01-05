import org.specs2.mutable.Specification

class ListSpec extends Specification {

  "companion object" should {
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

  "tail" should {
    "remove first element" in {
      List(1, 2, 3).tail mustEqual Cons(2, Cons(3, Nil))
    }
  }

  "drop" should {
    "remove 1 element from the list of 1" in {
      List(1).drop(1) mustEqual Nil
    }

    "remove 2 element from the list of 3" in {
      List(1, 2, 3).drop(2) mustEqual Cons(3, Nil)
    }

  }

  "drop while" should {

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

  "set head" should {
    "replace head in the list" in {
      List(1,2).setHead(3) mustEqual Cons(3, Cons(2, Nil))
    }
  }

  "init" should {

    "return Nil for list of size 1" in {
      List(1).init mustEqual Nil
    }

    "return first 2 elements for list of size 3" in {
      List(1,2,3).init mustEqual Cons(1, Cons(2, Nil))
    }
  }

}