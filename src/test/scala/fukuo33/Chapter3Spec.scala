package fukuo33

import org.specs2.mutable._

class Chapter3Spec extends SpecificationWithJUnit {

  "product" should {
    "return" in {
      List.product(List(1, 2, 3)) must_== 6
    }
  }

  /** Exercise 1 */
  "match" should {
    "return" in {
      val result = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }
      result must_== 3
    }
  }

  /** Exercise 2 */
  "tail" should {
    "return tail removed list" in {
      List.tail(List(1, 2, 3, 4, 5)) must_== List(2, 3, 4, 5)
    }
  }

  /** Exercise 3 */
  "setHead" should {
    "return head added list" in {
      List.setHead(1, List(2, 3, 4, 5)) must_== List(1, 2, 3, 4, 5)
    }
  }

  /** Exercise 4 */
  "drop" should {
    "return dropped list" in {
      List.drop(List(1, 2, 3, 4, 5), 3) must_== List(4, 5)
    }
  }

  /** Exercise 5 */
  "dropWhile" should {
    "return dropped list" in {
      List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x <= 3) must_== List(4, 5)
    }
  }

  /** Exercise 6 */
  "init" should {
    "return dropped list" in {
      List.init(List(1, 2, 3, 4, 5)) must_== List(1, 2, 3, 4)
    }
  }

  /** Exercise 6 */
  "init" should {
    "return dropped list" in {
      List.init(List(1, 2, 3, 4, 5)) must_== List(1, 2, 3, 4)
    }
  }

  /** Exercise 7 */
  "product3" should {
    "return product value" in {
      // short-circuiting might work?
      List.product3(List(1, 2, 3, 0, 4, 5)) must_== 0
    }
  }

  /** Exercise 8 */
  "foldRight" should {
    "return" in {
      // What do you think this says about the relationship between foldRight and the data constructors of List?
      List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) must_== Cons(1, Cons(2, Cons(3, Nil)))
    }
  }

  /** Exercise 9 */
  "length" should {
    "return length of list" in {
      List.length(List(1, 2, 3, 4, 5)) must_== 5
    }
  }

  /** Exercise 10 */
  "foldLeft" should {
    "return added value" in {
      List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) must_== 15
    }
    "return product value" in {
      List.foldLeft(List(1, 2, 3, 4, 5), 1)(_ * _) must_== 120
    }
  }

  /** Exercise 10 */
  "foldLeft" should {
    "return added value" in {
      List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) must_== 15
    }
    "return product value" in {
      List.foldLeft(List(1, 2, 3, 4, 5), 1)(_ * _) must_== 120
    }
  }

  /** Exercise 11 */
  "sumAtFoldLeft" should {
    "return summarized value" in {
      List.sumAtFoldLeft(List(1, 2, 3, 4, 5)) must_== 15
    }
  }
  "productAtFoldLeft" should {
    "return producted value" in {
      List.productAtFoldLeft(List(1, 2, 3, 4, 5)) must_== 120
    }
  }
  "lengthAtFoldLeft" should {
    "return tail removed list" in {
      List.lengthAtFoldLeft(List(1, 2, 3, 4, 5)) must_== 5
    }
  }

  /** Exercise 12 */
  "reverse" should {
    "return reverse list" in {
      List.reverse(List(1, 2, 3, 4, 5)) must_== List(5, 4, 3, 2, 1)
    }
  }

  /** Exercise 13 */
  "foldLeft2" should {
    // Can you write foldLeft in terms of foldRight?
    // * foldLeft
    // (List(1, 2, 3, 4, 5), 0)(_ + _)
    // (((((0 + 1) + 2) + 3) + 4) + 5)
    //
    // * foldRight
    // (List(1, 2, 3, 4, 5), 0)(_ + _)
    // 1 + (2 + (3 + (4 + (5 + 0))))
    // -> (((((0 + 5) + 4)) + 3) + 2) + 1

    "return added value" in {
      List.foldLeft2(List(1, 2, 3, 4, 5), 0)(_ + _) must_== 15
    }
    "return product value" in {
      List.foldLeft2(List(1, 2, 3, 4, 5), 1)(_ * _) must_== 120
    }
  }

  /** Exercise 14 */
  "append2" should {
    "return appended list" in {
      List.append(List(5, 6, 7), List(1, 2, 3)) must_== List(5, 6, 7, 1, 2, 3)
    }
  }

  /** Exercise 15 */
  "concat" should {
    "return concat list" in {
      List.concat(List(List(0, 9, 8), List(5, 6, 7), List(1, 2, 3))) must_== List(0, 9, 8, 5, 6, 7, 1, 2, 3)
    }
  }

  /** Exercise 16 */
  "addOne" should {
    "return add one list" in {
      List.addOne(List(1, 2, 3, 4, 5, 6)) must_== List(2, 3, 4, 5, 6, 7)
    }
  }

  /** Exercise 17 */
  "doubleToString" should {
    "return String list" in {
      List.doubleToString(List(1.0, 2.0, 3.0, 4.0, 5.0)) must_== List("1.0", "2.0", "3.0", "4.0", "5.0")
    }
  }

  /** Exercise 18 */
  "map" should {
    "return addOne list" in {
      List.map(List(1, 2, 3, 4, 5))(_ + 1) must_== List(2, 3, 4, 5, 6)
    }
    "return String list" in {
      List.map(List(1.0, 2.0, 3.0, 4.0, 5.0))(_.toString) must_== List("1.0", "2.0", "3.0", "4.0", "5.0")
    }
  }

  /** Exercise 19 */
  "filter" should {
    "return filtered list" in {
      List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) must_== List(2, 4, 6)
    }
  }

  /** Exercise 20 */
  "flatMap" should {
    "return apply function and flat list" in {
      List.flatMap(List(1, 2, 3))(i => List(i, i)) must_== List(1, 1, 2, 2, 3, 3)
    }
  }

  /** Exercise 21 */
  "filter2" should {
    "return filtered list" in {
      List.filter2(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) must_== List(2, 4, 6)
    }
  }

  /** Exercise 22 */
  "addCorrespondElement" should {
    "return added list" in {
      List.addCorrespondElement(List(1, 2, 3), List(2, 3, 4)) must_== List(3, 5, 7)
    }
  }

  /** Exercise 23 */
  "applyCorrespondElement" should {
    "return product list" in {
      List.applyCorrespondElement(List(1, 2, 3), List(2, 3, 4))(_ * _) must_== List(2, 6, 12)
    }
  }

  /** Exercise 24 */
  "hasSubsequence" should {
    "return true" in {
      List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3)) must_== true
    }
    "return true" in {
      List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) must_== true
    }
    "return false" in {
      List.hasSubsequence(List(1, 2, 3, 4), List(3, 2)) must_== false
    }
    "return false" in {
      List.hasSubsequence(List(1, 2, 3, 4), List(1, 3)) must_== false
    }
    "return true" in {
      List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) must_== true
    }
  }

  "Tree" should {
    /** Exercise 25 */
    "size" should {
      "return" in {
        Tree.size(Branch(Leaf(1), Leaf(2))) must_== 3
        Tree.size(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))) must_== 7
      }
    }

    /** Exercise 26 */
    "maximum" should {
      "return" in {
        Tree.maximum(Branch(Leaf(1), Leaf(2))) must_== 2
        Tree.maximum(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))) must_== 4
        Tree.maximum(Branch(Branch(Leaf(1), Branch(Leaf(10), Leaf(3))), Leaf(4))) must_== 10
      }
    }

    /** Exercise 27 */
    "depth" should {
      "return" in {

        Tree.depth(Branch(Leaf(1), Leaf(2))) must_== 2

        Tree.depth(
          Branch(
            Branch(
              Leaf(1),
              Branch(
                Leaf(1),
                Leaf(1))),
            Leaf(1))) must_== 4

        Tree.depth(
          Branch(
            Branch(
              Leaf(1),
              Branch(
                Branch(
                  Leaf(1),
                  Leaf(2)),
                Leaf(3))),
            Leaf(4))) must_== 5
      }
    }
  }

  /** Exercise 28 */
  "map" should {
    "return" in {
      Tree.map(Branch(Leaf(1), Leaf(2)))(a => a + 1) must_== Branch(Leaf(2), Leaf(3))

      Tree.map(
        Branch(
          Branch(
            Leaf("a"),
            Branch(
              Leaf("b"),
              Leaf("c"))),
          Leaf("d")))(a => a.toUpperCase) must_==
        Branch(
          Branch(
            Leaf("A"),
            Branch(
              Leaf("B"),
              Leaf("C"))),
          Leaf("D"))
    }
  }

  /** Exercise 29 */
  "foldRight" should {
    "return" in {
      Tree.fold(Branch(Leaf(1), Leaf(2)))(_ + 1)(_ + _) must_== 5
      Tree.fold(
        Branch(
          Branch(
            Leaf(1),
            Branch(
              Branch(
                Leaf(1),
                Leaf(2)),
              Leaf(3))),
          Leaf(4)))(_ + 1)(_ + _) must_== 16
    }
  }

}
