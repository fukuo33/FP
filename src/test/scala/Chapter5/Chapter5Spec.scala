package Chapter5

import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by fukuo33 on 2014/02/09.
 */
class Chapter5Spec extends SpecificationWithJUnit {

  "Stream#headOption" should {
    "return Some head when exist element" in {
      val stream = Stream.cons(1, Stream.cons({sys.error("fail"); 2}, Stream.cons(3, Stream.empty)))
      stream.headOption must_== Some(1)
    }
    "return None when not exist element" in {
      Stream().headOption must_== None
    }
  }

  "Stream#toList" should {
    "return List" in {
      Stream(1, 2, 3, 4, 5).toList must_== List(1, 2, 3, 4, 5)
    }
    "evaluate only once time" in {
      val s = Stream({println(1); 1}, {println(2); 2}, {println(3); 3})
      s.toList must_== List(1, 2, 3) // println is output
      s.toList must_== List(1, 2, 3) // println is not output.
    }
  }

  "Stream#take" should {
    "return take first n elements" in {
      val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons({sys.error("fail"); 4}, Stream.cons(5, Stream.empty)))))
      stream.take(3).toList must_== List(1, 2, 3)
    }
  }

  "Stream#drop" should {
    "return drop first n elements" in {
      val stream = Stream.cons(1, Stream.cons({sys.error("fail"); 2}, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.empty)))))
      stream.drop(3).toList must_== List(4, 5)
    }
  }

  "Stream#takeWhile" should {
    "return take" in {
      val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons({sys.error("fail"); 5}, Stream.empty)))))
      stream.takeWhile(_ < 4).toList must_== List(1, 2, 3)
    }
  }

  "Stream#exist" should {
    "return true" in {
      val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons({sys.error("fail"); 5}, Stream.empty)))))
      stream.exists(_ == 3) must_== true
    }
  }

  "Stream#existUseFoldRight" should {
    "return true" in {
      val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons({sys.error("fail"); 5}, Stream.empty)))))
      stream.existsUseFoldRight(_ == 3) must_== true
    }
  }

  "Stream#forAll" should {
    "return true when all elements match" in {
      val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.empty)))))
      stream.forAll(_ < 10) must_== true
    }
    "return false and early terminate when not match elements" in {
      val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons({sys.error("fail"); 5}, Stream.empty)))))
      stream.forAll(_ < 4) must_== false
    }
  }

  "Stream#headOptionUseFoldRight" should {
    "return Some head when exist element" in {
      val stream = Stream.cons(1, Stream.cons({sys.error("fail"); 2}, Stream.cons(3, Stream.empty)))
      stream.headOptionUseFoldRight must_== Some(1)
    }
    "return None when not exist element" in {
      Stream().headOptionUseFoldRight must_== None
    }
  }

  "Stream#map" should {
    "return function apply Stream" in {
      Stream(1, 2, 3, 4, 5).map(_ * 10).toList must_== List(10, 20, 30, 40, 50)
    }
    "return Empty when Empty" in {
      Stream[Int]().map(_ * 10).toList must_== List()
    }
  }

  "Stream#filter" should {
    "return filtered elements" in {
      Stream(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0).toList must_== List(2, 4, 6)
    }
  }

  "Stream#append" should {
    "return appended elements" in {
      Stream(1, 2, 3).append(Stream(4, 5, 6)).toList must_== List(1, 2, 3, 4, 5, 6)
    }
  }

  "Stream#flatMap" should {
    "return function apply and flat Stream" in {
      Stream(1, 2, 3).flatMap(a => Stream(a, a)).toList must_== List(1, 1, 2, 2, 3, 3)
    }
  }

  "Stream#find" should {
    "return find first element" in {
      val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons({sys.error("fail"); 5}, Stream.empty)))))
      stream.find(_ == 4) must_== Some(4)
    }
  }

  "Stream.constant" should {
    "return intinite Stream" in {
      Stream.constant("a").take(5).toList must_== List("a", "a", "a", "a", "a")
    }
  }

  "Stream.from" should {
    "return intinite Stream" in {
      Stream.from(10).take(5).toList must_== List(10, 11, 12, 13, 14)
    }
  }

  "Stream.fibs" should {
    "return intinite Stream" in {
      Stream.fibs.take(10).toList must_== List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }
  }

  "Stream.fibs2" should {
    "return intinite Stream" in {
      Stream.fibs2(0, 1).take(10).toList must_== List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }
  }

  "Stream.unfold" should {
    "return" in {
      Stream.unfold(10)(s => if (s == 0) None else Some((s, s-1))).toList must_== List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    }
  }

  "Stream.fibsUseUnfold" should {
    "return intinite Stream" in {
      Stream.fibsUseUnfold.take(10).toList must_== List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }
  }

  "Stream.fromUseUnfold" should {
    "return intinite Stream" in {
      Stream.fromUseUnfold(10).take(5).toList must_== List(10, 11, 12, 13, 14)
    }
  }

  "Stream.constantUseUnfold" should {
    "return intinite Stream" in {
      Stream.constantUseUnfold("a").take(5).toList must_== List("a", "a", "a", "a", "a")
    }
  }

  "Stream.onesUseUnfold" should {
    "return intinite Stream" in {
      Stream.onesUseUnfold.take(5).toList must_== List(1, 1, 1, 1, 1)
    }
  }

  "Stream#mapUseUnfold" should {
    "return function apply Stream" in {
      Stream(1, 2, 3, 4, 5).mapUseUnfold(_ * 10).toList must_== List(10, 20, 30, 40, 50)
    }
  }

  "Stream#takeUseUnfold" should {
    "return take first n elements" in {
      val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons({sys.error("fail"); 4}, Stream.cons(5, Stream.empty)))))
      stream.takeUseUnfold(3).toList must_== List(1, 2, 3)
    }
  }

  "Stream#takeWhile" should {
    "return take" in {
      val stream = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons({sys.error("fail"); 5}, Stream.empty)))))
      stream.takeWhileUseUnfold(_ < 4).toList must_== List(1, 2, 3)
    }
  }

  "Stream#zipWith" should {
    "return zip value" in {
      val stream1 = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons({sys.error("fail"); 5}, Stream.empty)))))
      val stream2 = Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.cons({sys.error("fail"); 6}, Stream.empty)))))
      stream1.zipWith(stream2) ((e1, e2) => e1 + e2).take(3).toList must_== List(3, 5, 7)

      // Why is take(4) to occur the error?
      //stream1.zipWith(stream2) ((e1, e2) => e1 + e2).take(4).toList must_== List(3, 5, 7, 9)
    }
    "return zip value from infinite list" in {
      val stream1 = Stream.fromUseUnfold(10)
      val stream2 = Stream.onesUseUnfold
      stream1.zipWith(stream2) ((e1, e2) => e1 + e2).take(4).toList must_== List(11, 12, 13, 14)
    }
  }

  "Stream#zipAll" should {
    "return zip value when stream1 is short" in {
      val stream1 = Stream.cons(1, Stream.cons(2, Stream.empty))
      val stream2 = Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons(5, Stream.cons({sys.error("fail"); 6}, Stream.empty)))))
      stream1.zipAll(stream2).take(3).toList must_== List((Some(1), Some(2)), (Some(2), Some(3)), (None, Some(4)))
    }

    "return zip value when stream2 is short" in {
      val stream1 = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(4, Stream.cons({sys.error("fail"); 5}, Stream.empty)))))
      val stream2 = Stream.cons(2, Stream.cons(3, Stream.empty))
      stream1.zipAll(stream2).take(3).toList must_== List((Some(1), Some(2)), (Some(2), Some(3)), (Some(3), None))
    }

    "return zip value from infinite list" in {
      val stream1 = Stream.fromUseUnfold(10)
      val stream2 = Stream.onesUseUnfold
      stream1.zipAll(stream2).take(4).toList must_== List((Some(10), Some(1)), (Some(11), Some(1)), (Some(12), Some(1)), (Some(13), Some(1)))
    }
  }

  "Stream#startsWith" should {
    "return true when starts" in {
      Stream(1, 2, 3).startsWith(Stream(1, 2)) must_== true
    }

    "return true when identical" in {
      Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3, 4, 5)) must_== true
    }

    "return false when not starts" in {
      Stream(1, 2, 3, 4, 5).startsWith(Stream(2, 3)) must_== false
    }

    "return false when original is short length" in {
      Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4, 5)) must_== false
    }
  }

  "Stream#tails" should {
    "return tails" in {
//      Stream(1, 2, 3).tails must_== Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())
      Stream(1, 2, 3).tails.toList.size must_== 3
      Stream(1, 2, 3).tails.toList(0).toList must_== List(1,2,3)
      Stream(1, 2, 3).tails.toList(1).toList must_== List(2,3)
      Stream(1, 2, 3).tails.toList(2).toList must_== List(3)
    }
  }

  "Stream#hasSubsequence" should {
    "return true" in {
      Stream(1, 2, 3, 4, 5).hasSubsequence(Stream(2, 3)) must_== true
    }
  }

  "Stream#scanRight" should {
    "return" in {
      Stream(1,2,3).scanRight(0)(_ + _).toList must_== List(1+2+3+0, 2+3+0, 3+0, 0)
    }
  }

}
