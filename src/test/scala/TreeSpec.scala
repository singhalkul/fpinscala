import org.specs2.mutable.Specification

class TreeSpec extends Specification {

  "25 Size of tree" should {

    "return size 1 for only 1 leaf" in {
      Leaf(1).size mustEqual 1
    }

    "return size 3 for 1 branch and 2 leaf" in {
      Branch(Leaf(1), Leaf(2)).size mustEqual 3
    }

    "return size 7 for full binary tree" in {
      Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))).size mustEqual 7
    }
  }

  "26 maximum int of tree" should {

    "return 2 for tree containing 1,2" in {
      Tree.maximum(Branch(Leaf(1), Leaf(2))) mustEqual 2
    }

    "return 6 for tree containing 1,6,3,4" in {
      Tree.maximum(Branch(Branch(Leaf(1), Leaf(6)), Branch(Leaf(3), Leaf(4)))) mustEqual 6
    }
  }

  "27 depth of tree" should {

    "return 0 for only leaf" in {
      Tree.depth(Leaf(1)) mustEqual 0
    }

    "return 1 for 1 branch and 2 leaf" in {
      Tree.depth(Branch(Leaf(1), Leaf(2))) mustEqual 1
    }

    "return 2 when right branch has more nodes" in {
      Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) mustEqual 2
    }
  }

  "28 map" should {
    "add 1 to all elements" in {
      Tree.map(Branch(Leaf(1), Leaf(2)))(_ + 1) mustEqual Branch(Leaf(2), Leaf(3))
    }
  }

  "29 fold" should {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

    "calculate size" in {
      Tree.fold(tree)(a => 1)(_ + _ + 1) mustEqual 7
    }

    "calculate depth" in {
      Tree.fold(tree)(a => 0)((l,r) => 1 + (l max r)) mustEqual 2
    }

    "map values" in {
      val tree = Branch(Leaf(1), Leaf(2))
      Tree.fold(tree)(a => Leaf(a + 1): Tree[Int])((l, r) => Branch(l, r)) mustEqual Branch(Leaf(2), Leaf(3))
    }
  }
}
