package Chapter2

import org.specs2.mutable._

class Chapter2Spec extends SpecificationWithJUnit {

  /** Exercise 1 */
  "fib" should {
    "return" in {
      Chapter2.fib(1) must_== 1
      Chapter2.fib(2) must_== 2
      Chapter2.fib(3) must_== 3
      Chapter2.fib(4) must_== 5
      Chapter2.fib(5) must_== 8
      Chapter2.fib(6) must_== 13
      Chapter2.fib(7) must_== 21
      Chapter2.fib(8) must_== 34
      Chapter2.fib(9) must_== 55
      Chapter2.fib(10) must_== 89
    }
  }


}
