package Chapter5

import Chapter2._

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

  println("==================")

  println(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList)
// trace
//  Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList // Apply map to the first element.
//  cons(11, Stream(2,3,4).map(_ + 10)).filter(_ % 2 == 0).toList  // Apply filter to the first element.
//  Stream(2,3,4).map(_ + 10).filter(_ % 2 == 0).toList // Apply map to the second element.
//  cons(12, Stream(3,4).map(_ + 10)).filter(_ % 2 == 0).toList // Apply filter to the second element. Produce the first element of the result.
//  12 :: Stream(3,4).map(_ + 10).filter(_ % 2 == 0).toList
//  12 :: cons(13, Stream(4).map(_ + 10)).filter(_ % 2 == 0).toList
//  12 :: Stream(4).map(_ + 10).filter(_ % 2 == 0).toList
//  12 :: cons(14, Stream().map(_ + 10)).filter(_ % 2 == 0).toList
//  12 :: 14 :: Stream().map(_ + 10).filter(_ % 2 == 0).toList  // Apply filter to the fourth element and produce the final element of the result.
//  12 :: 14 :: List() // map and filter have no more work to do and the empty stream becomes the empty list.

  // ここの考え方
  // Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList の場合
  //
  // 下記のようにmapの関数が適用される。この時名前渡しなので評価はされない。
  // Stream(f(1 + 10), f(2 + 10), f(3 + 10), f(4 + 10)).filter(_ % 2 == 0).toList
  //
  // 次にfilterの関数が適用される。同様に評価はされない。
  // Stream(g(f(1 + 10) % 2 == 0), g(f(2 + 10) % 2 == 0), g(f(3 + 10) % 2 == 0), g(f(4 + 10)) % 2 == 0).toList
  //
  // toListのタイミングで評価が始まる。
  // 12 :: 14 == List(12, 14)

  println(Stream(1,2,3,4).map(i => {println(i); i + 10}).take(2).toList) // Wow!

  println("==================")

  val ones: Stream[Int] = Stream.cons(1, ones)
  println(ones.take(5).toList)
  println(ones.exists(_ % 2 != 0))

  println(ones.map(_ + 1).exists(_ % 2 == 0))
  println(ones.takeWhile(_ == 1))
  println(ones.forAll(_ != 1))

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

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOptionUseFoldRight: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty: Stream[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty: Stream[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[B>:A](other: => Stream[B]): Stream[B] = foldRight(other)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = map(f).foldRight(Stream.empty: Stream[B])((a, b) => a.append(b))
//  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty: Stream[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def f(n: Int): Stream[Int] = Stream.cons(Chapter2.fib(n), f(n + 1))
    f(0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.empty
      case Some((cur, next)) => Stream.cons(cur, unfold(next)(f))
    }
  }
}