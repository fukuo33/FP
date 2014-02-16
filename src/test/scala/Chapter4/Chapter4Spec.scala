package Chapter4

import org.specs2.mutable._
import java.lang.NumberFormatException

class Chapter4Spec extends SpecificationWithJUnit {

  /** Exercise 1 */
  "Option" should {

    "map" in {
      (Some(15): Option[Int]).map(_ + 5) must_== Some(20)
      (None: Option[Int]).map(_ + 5) must_== None
    }

    "flatMap" in {
      (Some(15): Option[Int]).flatMap(v => Some(v + 5)) must_== Some(20)
      (None: Option[Int]).flatMap(v => Some(v + 5)) must_== None
    }

    "getOrElse" in {
      (Some(15): Option[Int]).getOrElse(3) must_== 15
      (None: Option[Int]).getOrElse(3) must_== 3
    }

    "orElse" in {
      (Some(15): Option[Int]).orElse(Some(3)) must_== Some(15)
      (None: Option[Int]).orElse(Some(3)) must_== Some(3)
    }

    "filter" in {
      (Some(15): Option[Int]).filter(v => v % 2 == 0) must_== None
      (Some(16): Option[Int]).filter(v => v % 2 == 0) must_== Some(16)
      (None: Option[Int]).filter(v => v % 2 == 0) must_== None
    }

    /** Exercise 3 */
    "map2" should {
      "return Some when a and b both Some" in {
        val i: Option[Int] = Some(1)
        val j: Option[Int] = Some(2)
        Option.map2(i, j)((a, b) => a + b) must_== Some(3)
      }
      "return None when a is None" in {
        val i: Option[Int] = None
        val j: Option[Int] = Some(2)
        Option.map2(i, j)((a, b) => a + b) must_== None
      }
      "return None when b is None" in {
        val i: Option[Int] = Some(1)
        val j: Option[Int] = None
        Option.map2(i, j)((a, b) => a + b) must_== None
      }
    }

    /** Exercise 4 */
    "sequence" should {
      "return List[Some] when all value is Some" in {
        val l = Some(1) :: Some(2) :: Some(3) :: Nil
        Option.sequence(l) must_== Some(1 :: 2 :: 3 :: Nil)
      }
      "return None when last element is None" in {
        val l = Some(1) :: Some(2) :: None :: Nil
        Option.sequence(l) must_== None
      }
      "return None when None even one" in {
        val l = Some(1) :: None :: Some(3) :: Nil
        Option.sequence(l) must_== None
      }
    }
    "sequenceR" should {
      "return List[Some] when all value is Some" in {
        val l = Some(1) :: Some(2) :: Some(3) :: Nil
        Option.sequenceR(l) must_== Some(1 :: 2 :: 3 :: Nil)
      }
      "return None when last element is None" in {
        val l = Some(1) :: Some(2) :: None :: Nil
        Option.sequenceR(l) must_== None
      }
      "return None when None even one" in {
        val l = Some(1) :: None :: Some(3) :: Nil
        Option.sequenceR(l) must_== None
      }
    }
    "Option.smartSequence" should {
      "return List[Some] when all value is Some" in {
        val l = Some(1) :: Some(2) :: Some(3) :: Nil
        Option.smartSequence(l) must_== Some(1 :: 2 :: 3 :: Nil)
      }
      "return None when last element is None" in {
        val l = Some(1) :: Some(2) :: None :: Nil
        Option.smartSequence(l) must_== None
      }
      "return None when None even one" in {
        val l = Some(1) :: None :: Some(3) :: Nil
        Option.smartSequence(l) must_== None
      }
    }

    /** Exercise 5 */
    "traverse" should {
      "return Some(List) when all value is Some" in {
        val l = "1" :: "2" :: "3" :: Nil
        Option.traverse(l)(i => Option.Try(i.toInt)) must_== Some(1 :: 2 :: 3 :: Nil)
      }
      "return None when None even one" in {
        val l = "1" :: "a" :: "3" :: Nil
        Option.traverse(l)(i => Option.Try(i.toInt)) must_== None
      }
    }
    "smartTraverse" should {
      "return Some(List) when all value is Some" in {
        val l = "1" :: "2" :: "3" :: Nil
        Option.smartTraverse(l)(i => Option.Try(i.toInt)) must_== Some(1 :: 2 :: 3 :: Nil)
      }
      "return None when None even one" in {
        val l = "1" :: "a" :: "3" :: Nil
        Option.smartTraverse(l)(i => Option.Try(i.toInt)) must_== None
      }
    }

  }

  "abs0" should {
    "return abs value" in {
      Chapter4.absO(Some(-12.3)) must_== Some(12.3)
      Chapter4.absO(Some(12.3)) must_== Some(12.3)
      Chapter4.absO(None) must_== None
    }
  }

  "parseInsuranceRateQuote" should {
    "return rate when a and b is Int" in {
      InsuranceRate.parseInsuranceRateQuote("35", "2") must_== Some(70)
    }
    "return rate when a is not Int" in {
      InsuranceRate.parseInsuranceRateQuote("one", "2") must_== None
    }
    "return rate when b is not Int" in {
      InsuranceRate.parseInsuranceRateQuote("35", "two") must_== None
    }
  }


  /** Exercise 6 */
  "Either" should {

    "map" in {
      (Right(15): Either[String, Int]).map(_ + 5) must_== Right(20)
      (Left("error message"): Either[String, Int]).map(_ + 5) must_== Left("error message")
    }

    "flatMap" in {
      (Right(15): Either[String, Int]).flatMap(v => Right(v + 5)) must_== Right(20)
      (Left("error message"): Either[String, Int]).flatMap(v => Right(v + 5)) must_== Left("error message")
    }

    "orElse" in {
      (Right(15): Either[String, Int]).orElse(Right(3)) must_== Right(15)
      (Left("error message"): Either[String, Int]).orElse(Right(3)) must_== Right(3)
    }

    "map2" should {
      "return Right when a and b both Right" in {
        val i: Either[String, Int] = Right(1)
        val j: Either[String, Int] = Right(2)
        i.map2(j)((a, b) => a + b) must_== Right(3)
      }
      "return Left when a is Left" in {
        val i: Either[String, Int] = Left("error message")
        val j: Either[String, Int] = Right(2)
        i.map2(j)((a, b) => a + b) must_== Left("error message")
      }
      "return Left when b is Left" in {
        val i: Either[String, Int] = Right(1)
        val j: Either[String, Int] = Left("error message")
        i.map2(j)((a, b) => a + b) must_== Left("error message")
      }
    }

    /** Exercise 7 */
    "sequence" should {
      "return List[Right] when all value is Right" in {
        val l = Right(1) :: Right(2) :: Right(3) :: Nil
        Either.sequence(l) must_== Right(1 :: 2 :: 3 :: Nil)
      }
      "return Left when last element is Left" in {
        val l = Right(1) :: Right(2) :: Left("error message") :: Nil
        Either.sequence(l) must_== Left("error message")
      }
      "return first Left when multiple Left" in {
        val l = Right(1) :: Left("error message") :: Left("error message2") :: Nil
        Either.sequence(l) must_== Left("error message")
      }
    }

    "traverse" should {
      "return Right(List) when all value is Right" in {
        val l = "1" :: "2" :: "3" :: Nil
        Either.traverse(l)(i => Either.Try(i.toInt)) must_== Right(1 :: 2 :: 3 :: Nil)
      }
      // TODO: How to implement beLeft?
      //    "return Left when Left even one" in {
      //      val l = "1" :: "a" :: "3" :: Nil
      //      Chapter4.traverseWithEither(l)(i => Chapter4.TryWithEither(i.toInt)) must beLeft
      //      (t => new NumberFormatException("For input string: \"a\""))
      //    }
    }

  }


  "mkPerson" should {
    "return Left when name is invalid" in {
      Chapter4.mkPerson("", 33) must_== Left("Name is empty.")
    }
    "return Left when age is invalid" in {
      Chapter4.mkPerson("makoto", -5) must_== Left("Age is out of range.")
    }
    "return Left when both the name and age are invalid" in {
      Chapter4.mkPerson("", -5) must_== Left("Name is empty.")
    }
    "return Right when arguments valid value" in {
      Chapter4.mkPerson("makoto", 33) must_== Right(Person(Name("makoto"), Age(33)))
    }
  }

  /** Exercise 8 */
  "mkPerson2" should {
    "return Left when name is invalid" in {
      Chapter4.mkPerson2("", 33) must_== Left(List("Name is empty."))
    }
    "return Left when age is invalid" in {
      Chapter4.mkPerson2("makoto", -5) must_== Left(List("Age is out of range."))
    }
    "return Left when both the name and age are invalid" in {
      Chapter4.mkPerson2("", -5) must_== Left(List("Name is empty.", "Age is out of range."))
    }
    "return Right when arguments valid value" in {
      Chapter4.mkPerson("makoto", 33) must_== Right(Person(Name("makoto"), Age(33)))
    }
  }
  "Pertial" should {

    "orElse" in {
      (Success(15): Partial[String, Int]).orElse(Success(3)) must_== Success(15)
      (Errors(Seq("error message")): Partial[String, Int]).orElse(Success(3)) must_== Success(3)
    }

    "map2" should {
      "return Success when a and b both Success" in {
        val i: Partial[String, Int] = Success(1)
        val j: Partial[String, Int] = Success(2)
        i.map2(j)((a, b) => a + b) must_== Success(3)
      }
      "return Errors when a is Errors" in {
        val i: Partial[String, Int] = Errors(Seq("error message1"))
        val j: Partial[String, Int] = Success(2)
        i.map2(j)((a, b) => a + b) must_== Errors(Seq("error message1"))
      }
      "return Errors when b is Errors" in {
        val i: Partial[String, Int] = Success(1)
        val j: Partial[String, Int] = Errors(Seq("error message2"))
        i.map2(j)((a, b) => a + b) must_== Errors(Seq("error message2"))
      }
      "return Errors when a and b both Errors" in {
        val i: Partial[String, Int] = Errors(Seq("error message1"))
        val j: Partial[String, Int] = Errors(Seq("error message2"))
        i.map2(j)((a, b) => a + b) must_== Errors(Seq("error message1", "error message2"))
      }
    }

    "sequence" should {
      "return List[Success] when all value is Success" in {
        val l = Success(1) :: Success(2) :: Success(3) :: Nil
        Partial.sequence(l) must_== Success(1 :: 2 :: 3 :: Nil)
      }
      "return Errors when last element is Errors" in {
        val l = Success(1) :: Success(2) :: Errors(Seq("error message1")) :: Nil
        Partial.sequence(l) must_== Errors(Seq("error message1"))
      }
      "return first Errors when multiple Errors" in {
        val l = Success(1) :: Errors(Seq("error message1")) :: Errors(Seq("error message2")) :: Nil
        Partial.sequence(l) must_== Errors(Seq("error message1", "error message2"))
      }
    }

    "traverse" should {
      "return Success when all Success" in {
        val l = "1" :: "2" :: "3" :: "4" :: Nil
        Partial.traverse(l)(i => Partial.Try(i.toInt)) must_== Success(List(1, 2, 3, 4))
      }

      // TODO: How to implement beLeft?
//      "return Errors" in {
//        val l = "1" :: "a" :: "b" :: "4" :: Nil
//        Partial.traverse(l)(i => Partial.Try(i.toInt)) must_== Errors(Seq(new NumberFormatException("For input string: \"a\""), new NumberFormatException("For input string: \"b\"")))
//      }
    }

  }

}
