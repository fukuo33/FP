package Chapter4

import org.specs2.mutable._

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

  }

  "abs0" should {
    "return abs value" in {
      Chapter4.absO(Some(-12.3)) must_== Some(12.3)
      Chapter4.absO(Some(12.3)) must_== Some(12.3)
      Chapter4.absO(None) must_== None
    }
  }

  /** Exercise 3 */
  "map2" should {
    "return Some when a and b both Some" in {
      val i: Option[Int] = Some(1)
      val j: Option[Int] = Some(2)
      Chapter4.map2(i, j)((a, b) => a + b) must_== Some(3)
    }
    "return None when a is None" in {
      val i: Option[Int] = None
      val j: Option[Int] = Some(2)
      Chapter4.map2(i, j)((a, b) => a + b) must_== None
    }
    "return None when b is None" in {
      val i: Option[Int] = Some(1)
      val j: Option[Int] = None
      Chapter4.map2(i, j)((a, b) => a + b) must_== None
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

  /** Exercise 4 */
  "sequence" should {
    "return List[Some] when all value is Some" in {
      val l = Some(1) :: Some(2) :: Some(3) :: Nil
      Chapter4.sequence(l) must_== Some(1 :: 2 :: 3 :: Nil)
    }
    "return None when last element is None" in {
      val l = Some(1) :: Some(2) :: None :: Nil
      Chapter4.sequence(l) must_== None
    }
    "return None when None even one" in {
      val l = Some(1) :: None :: Some(3) :: Nil
      Chapter4.sequence(l) must_== None
    }
  }
  "smartSequence" should {
    "return List[Some] when all value is Some" in {
      val l = Some(1) :: Some(2) :: Some(3) :: Nil
      Chapter4.smartSequence(l) must_== Some(1 :: 2 :: 3 :: Nil)
    }
    "return None when last element is None" in {
      val l = Some(1) :: Some(2) :: None :: Nil
      Chapter4.smartSequence(l) must_== None
    }
    "return None when None even one" in {
      val l = Some(1) :: None :: Some(3) :: Nil
      Chapter4.smartSequence(l) must_== None
    }
  }

  /** Exercise 5 */
  "traverse" should {
    "return Some(List) when all value is Some" in {
      val l = "1" :: "2" :: "3" :: Nil
      Chapter4.traverse(l)(i => Chapter4.Try(i.toInt)) must_== Some(1 :: 2 :: 3 :: Nil)
    }
    "return None when None even one" in {
      val l = "1" :: "a" :: "3" :: Nil
      Chapter4.traverse(l)(i => Chapter4.Try(i.toInt)) must_== None
    }
  }
  "smartTraverse" should {
    "return Some(List) when all value is Some" in {
      val l = "1" :: "2" :: "3" :: Nil
      Chapter4.smartTraverse(l)(i => Chapter4.Try(i.toInt)) must_== Some(1 :: 2 :: 3 :: Nil)
    }
    "return None when None even one" in {
      val l = "1" :: "a" :: "3" :: Nil
      Chapter4.smartTraverse(l)(i => Chapter4.Try(i.toInt)) must_== None
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
  }

  /** Exercise 7 */
  "sequenceWithEither" should {
    "return List[Right] when all value is Right" in {
      val l = Right(1) :: Right(2) :: Right(3) :: Nil
      Chapter4.sequenceWithEither(l) must_== Right(1 :: 2 :: 3 :: Nil)
    }
    "return Left when last element is Left" in {
      val l = Right(1) :: Right(2) :: Left("error message") :: Nil
      Chapter4.sequenceWithEither(l) must_== Left("error message")
    }
    "return first Left when multiple Left" in {
      val l = Right(1) :: Left("error message") :: Left("error message2") :: Nil
      Chapter4.sequenceWithEither(l) must_== Left("error message")
    }
  }

  "traverseWithEither" should {
    "return Right(List) when all value is Right" in {
      val l = "1" :: "2" :: "3" :: Nil
      Chapter4.traverseWithEither(l)(i => Chapter4.TryWithEither(i.toInt)) must_== Right(1 :: 2 :: 3 :: Nil)
    }
    "return Left when Left even one" in {
      val l = "1" :: "a" :: "3" :: Nil
      Chapter4.traverseWithEither(l)(i => Chapter4.TryWithEither(i.toInt)) must be equalTo Left(new NumberFormatException("For input string: \"a\""))
    }
  }

}
