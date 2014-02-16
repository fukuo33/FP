package Chapter5

import org.specs2.mutable.SpecificationWithJUnit

/**
 * Created by fukuo33 on 2014/02/09.
 */
class Chapter5Spec extends SpecificationWithJUnit {

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
      Stream(1, 2, 3, 4, 5).take(3).toList must_== List(1, 2, 3)
    }
  }

  "Stream#drop" should {
    "return drop first n elements" in {
      Stream(1, 2, 3, 4, 5).drop(3).toList must_== List(4, 5)
    }
  }

  "Stream#takeWhile" should {
    "return take" in {
      Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList must_== List(1, 2, 3)
    }
  }



}
