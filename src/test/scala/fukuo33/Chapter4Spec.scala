package fukuo33

import org.specs2.mutable._

class Chapter4Spec extends SpecificationWithJUnit {

  /** Exercise 1 */
  "Option" should {

    "map" in {
      Some(15).map(_ + 5) must_== Some(20)
      None.map(v => v) must_== None
    }

    "flatMap" in {
      Some(15).flatMap(v => Some(v + 5)) must_== Some(20)
      None.flatMap(v => Some(v)) must_== None
    }

    "getOrElse" in {
      Some(15).getOrElse(3) must_== 15
      None.getOrElse(3) must_== 3
    }

    "orElse" in {
      Some(15).orElse(Some(3)) must_== Some(15)
      None.orElse(Some(3)) must_== Some(3)
    }

    "filter" in {
      Some(15).filter(v => v % 2 == 0) must_== None
      Some(16).filter(v => v % 2 == 0) must_== Some(16)
      None.filter(v => true) must_== None
      None.filter(v => false) must_== None
    }

  }

}
