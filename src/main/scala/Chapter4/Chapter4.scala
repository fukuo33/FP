package Chapter4

/**
 * Created by fukuo33 on 2013/12/23.
 */
object  Chapter4 extends App {
  println("Wellcome to Chapter4.")

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  def mean_2(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  case class Employee(name: String, department: String)
  def lookupByName(name: String): Option[Employee] = name match {
    case "Joe" => Some(Employee("Joe", "Accounting"))
    case _ => None
  }
  val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)
  val terryDepartment: Option[String] = lookupByName("Terry").map(_.department)

  val dept: String =
    lookupByName("Joe").
      map(_.department).
      filter(_ != "Accounting").
      getOrElse("Default Dept")

  println(s"$joeDepartment, $terryDepartment, $dept")

  def variance(xs: Seq[Double]): Option[Double] = mean_2(xs) flatMap (m => mean_2(xs.map(x => math.pow(x - m, 2))))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f //(x) => x map {y => f(y)}

  def absO: Option[Double] => Option[Double] = lift(math.abs)

  def parseInts(a: List[String]): Option[List[Int]] = Option.sequence(a map (i => Option.Try(i.toInt)))

  def meanWithEither(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person)

  def mkPerson2(name: String, age: Int): Either[List[String], Person] =
    mkName(name).map2HaveMultiLeft(mkAge(age))(Person)
}

object InsuranceRate {
  /**
   * Top secret formula for computing an annual car
   * insurance premium from two key factors.
   */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double ={
    age * numberOfSpeedingTickets
  }

  def parseInsuranceRateQuote(
                               age: String,
                               numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Option.Try { age.toInt }
    val optTickets: Option[Int] = Option.Try { numberOfSpeedingTickets.toInt }
    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

  def parseInsuranceRateQuoteWithEither(
                               age: String,
                               numberOfSpeedingTickets: String): Either[Exception,Double] =
    for {
      a <- Either.Try { age.toInt }
      tickets <- Either.Try { numberOfSpeedingTickets.toInt }
    } yield insuranceRateQuote(a, tickets)

}

object Option {

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for (aValue <- a; bValue <- b) yield f(aValue, bValue)
  }

  // foldLeft style
  // List(Option(1), Option(2), Option(3)) ===> (Option(3) :: (Option(2) :: (Option(1) :: Nil))).reverse
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def sub(list: List[Option[A]], acc: List[A]): Option[List[A]] = list match {
      case Nil => Some(acc)
      case head :: tail => head flatMap(h => sub(tail, h :: acc))
    }
    sub(a, Nil).map (_.reverse)
  }

  // foldRight style
  // List(Option(1), Option(2), Option(3)) ===> (Option(1) :: (Option[(2) :: (Option(3) :: Nil)))
  def sequenceR[A](a: List[Option[A]]): Option[List[A]] =  a match {
    case Nil => Some(Nil)
    case h :: t => map2(h, sequenceR(t))(_ :: _)
  }

  // smart code
  def smartSequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]]){(x, acc) => map2(x, acc)(_ :: _)}
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def sub(list: List[A], acc: List[B]): Option[List[B]] = list match {
      case Nil => Some(acc)
      case head :: tail => f(head) flatMap(h => sub(tail, h :: acc))
    }
    sub(a, Nil).map (_.reverse)
  }

  // smart code
  def smartTraverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(Nil): Option[List[B]]){(x, acc) => map2(f(x), acc)(_ :: _)}
  }
}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(value) => f(value)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(value) => Some(value)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(value) if f(value) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    a.foldRight(Right(Nil): Either[E, List[A]]){(x, acc) => x.map2(acc)(_ :: _)}
  }

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    a.foldRight(Right(Nil): Either[E, List[B]]){(x, acc) => f(x).map2(acc)(_ :: _)}
  }
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(value) => Right(f(value))
    case Left(value) => Left(value)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case Left(value) => Left(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => Right(value)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for(aValue <- this; bValue <- b) yield f(aValue, bValue)
  }

  def map2HaveMultiLeft[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] = {
    (this, b) match {
      case (Right(aValue), Right(bValue)) => Right(f(aValue, bValue))
      case (Left(aValue), Left(bValue)) => Left(List(aValue, bValue))
      case (Left(aValue), _) => Left(List(aValue))
      case (_, Left(bValue)) => Left(List(bValue))
    }
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


case class Person(name: Name, age: Age)
sealed case class Name(value: String)
sealed case class Age(value: Int)


object Partial {
  def Try[A](a: => A): Partial[Exception, A] =
    try Success(a)
    catch { case e: Exception => Errors(Seq(e)) }

  def sequence[E, A](a: List[Partial[E, A]]): Partial[E, List[A]] = {
    a.foldRight(Success(Nil): Partial[E, List[A]]){(x, acc) => x.map2(acc)(_ :: _)}
  }

  def traverse[E, A, B](a: List[A])(f: A => Partial[E, B]): Partial[E, List[B]] = {
    a.foldRight(Success(Nil): Partial[E, List[B]]){(x, acc) => f(x).map2(acc)(_ :: _)}
  }
}

trait Partial[+E, +A] {

  def orElse[EE >: E, B >: A](b: => Partial[EE, B]): Partial[EE, B] = this match {
    case Success(value) => Success(value)
    case Errors(_) => b
  }

  def map2[EE >: E, B, C](b: Partial[EE, B])(f: (A, B) => C): Partial[EE, C] = {
    (this, b) match {
      case (Success(aValue), Success(bValue)) => Success(f(aValue, bValue))
      case (Errors(aValue), Errors(bValue)) => Errors(aValue ++ bValue)
      case (Errors(aValue), _) => Errors(aValue)
      case (_, Errors(bValue)) => Errors(bValue)
    }
  }

}

sealed case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]
sealed case class Success[+B](get: B) extends Partial[Nothing, B]