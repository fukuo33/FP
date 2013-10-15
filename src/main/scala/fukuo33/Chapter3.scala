package fukuo33

/**
 * Created with IntelliJ IDEA.
 * User: fukuo33
 * Date: 2013/10/14
 * Time: 17:46
 * To change this template use File | Settings | File Templates.
 */
object Chapter3 extends App {
  val ex1: List[Double] = Nil

  val ex2: List[Int] = Cons(1, Nil)

  val ex3: List[String] = Cons("a", Cons("b", Nil))

  println(List.product(List(1, 2, 3)))

  /**
   * Exercise 1
   */
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  println(s"Exercise1: $x")

  val e2 = List.tail(List(1, 2, 3, 4, 5))
  println(s"Exercise2: $e2")

  val e3 = List.setHead(1, List(2, 3, 4, 5))
  println(s"Exercise3: $e3")

  val e4 = List.drop(List(1, 2, 3, 4, 5), 3)
  println(s"Exercise4: $e4")

  val e5 = List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x <= 3)
  println(s"Exercise5: $e5")

  val appendList = List.append(List(1,2,3), List(4,5,6))
  println(s"appendList: $appendList")

  val e6 = List.init(List(1, 2, 3, 4, 5))
  println(s"Exercise6: $e6")

}


sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
   * Exercise 2
   */
  def tail[A](list: List[A]): List[A] = list match {
    case Cons(x, xs) => xs
    case _ => Nil
  }

  /**
   * Exercise 3
   */
  def setHead[A](a: A, list: List[A]): List[A] = list match {
    case Nil => Nil
    case xs => Cons(a, xs)
  }

  /**
   * Exercise 4
   */
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case num if num <= 0 => l
    case _ => drop(List.tail(l), n - 1)
  }

  /**
   * Exercise 5
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  /**
   * Exercise 6
   */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

}