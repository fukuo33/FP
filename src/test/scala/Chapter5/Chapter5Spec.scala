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


}
