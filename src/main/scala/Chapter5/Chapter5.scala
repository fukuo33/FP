package Chapter5

import scala.collection.immutable.Stream.cons

/**
 * Created by fukuo33 on 2014/02/09.
 */
object Chapter5 extends App {
  println("Wellcome to Chapter5.")

  def square(x: Double): Double = x * x

  false && { println("!!"); true } // does not print anything
  true || { println("!!"); false } // doesn't print anything either


  println("==================")

  // use function call
  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = if (cond) onTrue() else onFalse()
  val a = 11
  if2(a < 22,
    () => println("a"), // The function literal syntax for creating an () => A
    () => println("b")
  )

  println("==================")

  // use call by name
  def if3[A](cond: Boolean, onTrue: => A, onFalse: => A): A = if (cond) onTrue else onFalse
  val x = if3(false, sys.error("fail"), 3)
  println(x)


  println("==================")

  // That is, Scala won't (by default) cache the result of evaluating an argument
  def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0
  val xx = maybeTwice(true, { println("hi"); 1+41 })
  println(xx)

  println("==================")

  // We can cache the value explicitly if we wish to only evaluate the result once
  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j+j else 0
  }
  val xxx = maybeTwice2(true, { println("hi"); 1+41 })
  println(xxx)

  println("==================")

  Stream.cons(1, Stream.cons({sys.error("fail"); 2}, Stream.cons(3, Stream.empty)))


}


// === FP in Scala v12 ===
//sealed abstract class Stream[+A] {
//  def uncons: Option[Cons[A]]
//  def isEmpty: Boolean = uncons.isEmpty
//}
//object Empty extends Stream[Nothing] {
//  val uncons = None
//}
//sealed abstract class Cons[+A] extends Stream[A] {
//  def head: A
//  def tail: Stream[A]
//  val uncons = Some(this)
//}
//object Stream {
//  def empty[A]: Stream[A] = Empty
//  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
//    lazy val head = hd
//    lazy val tail = tl
//  }
//  def apply[A](as: A*): Stream[A] =
//    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
//}

// === FP in Scala v13 ===
sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    def f(s: Stream[A]): List[A] = s match {
      case Empty => Nil
      case Cons(h, t) => h() :: f(t())
    }
    f(this)
  }

  def take(n: Int): Stream[A] = {
    def f(s: Stream[A], count: Int): Stream[A] = s match {
      case Empty => Empty
      case Cons(_, _) if count > n => Empty
      case Cons(h, t) => Stream.cons(h(), f(t(), count + 1))
    }
    f(this, 1)
  }

  def drop(n: Int): Stream[A] = {
    def f(s: Stream[A], count: Int): Stream[A] = s match {
      case Empty => Empty
      case Cons(_, t) if count < n + 1 => f(t(), count + 1)
      case Cons(h, t) => Stream.cons(h(), f(t(), count + 1))
    }
    f(this, 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def f(s: Stream[A]): Stream[A] = s match {
      case Empty => Empty
      case Cons(h, t) if p(h()) => Stream.cons(h(),f(t()))
      case Cons(_, _) => Empty
    }
    f(this)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, t) => p(h()) || t().exists(p)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
  }

  def existsUseFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

}

case object Empty extends Stream[Nothing]

// 引数が関数渡し
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // 引数が名前渡し
  // 値の評価がキャッシュされる
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A* ): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}