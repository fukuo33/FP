package fukuo33

import scala.collection.immutable.{List => ScalaList}
import scala.collection.immutable.{Nil => ScalaNil}

/**
 * Created with IntelliJ IDEA.
 * User: fukuo33
 * Date: 2013/10/14
 * Time: 16:11
 * To change this template use File | Settings | File Templates.
 */
object Chapter2 extends App {

  println("Hello Chapter2 !")
  println(formatAbs(-42))
  println(factorial(7))

  val first = findFirst(Array(7, 9, 13), (x: Int) => x == 13)
  println(s"first:$first")

  println(isSorted(ScalaList(1, 2, 10, 4, 5), (x: Int, y: Int) => x < y))
  println(isSorted(ScalaList(1, 3, 4, 10), (x: Int, y: Int) => x < y))

  val x1 = curry((a: Int, b: Int) => a * b)
  val x2 = uncurry((a: Int) => (b: Int) => a * b)
  val x3 = compose((b: Int) => b * 3, (a: Int) => a * 2)

  println(x1(2)(3))
  println(x2(2, 3))
  print(x3(5))


  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def findFirst[A](ds: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ds.length) -1
      else if (p(ds(n))) n
      else loop(n + 1)
    loop(0)
  }

  /**
   * Exercise2
   */
  def isSorted[A](as: ScalaList[A], gt: (A,A) => Boolean): Boolean = as match {
    case x :: ScalaNil => true
    case x :: xs if gt(x, xs.head) => isSorted(xs, gt)
    case _ => false
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
  }

  /**
   * Exercise3
   */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  /**
   * Exercise4
   */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * Exercise5
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}