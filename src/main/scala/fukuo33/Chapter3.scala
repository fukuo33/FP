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

  def tail[A](list: List[A]): List[A] = list match {
    case Cons(x, xs) => xs
    case _ => Nil
  }

  def setHead[A](a: A, list: List[A]): List[A] = list match {
    case Nil => Nil
    case xs => Cons(a, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case num if num <= 0 => l
    case _ => drop(List.tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) = foldRight(l, 0)((x,y) => x + y)

  def product2(l: List[Double]) = foldRight(l, 1.0)(_ * _)

  def product3(l: List[Double]): Double = l match {
    case Nil => 1.0
    case Cons(x, xs) if x == 0.0 => 0.0
    case Cons(x, xs) =>
      println(s"product3: $x")
      x * product3(xs)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

  def foldLeft[A,B](l: List[A], acc: B)(f: (B, A) => B): B = l match {
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x))(f)
  }

  def sumAtFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productAtFoldLeft(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def lengthAtFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  def foldLeft2[A,B](l: List[A], acc: B)(f: (B, A) => B): B = foldRight(reverse(l), acc)((b, a) => f(a, b))

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a2, a1)((b, a) => Cons(a, b))

  def concat[A](lists: List[List[A]]): List[A] = foldRight(lists, List[A]())((a, b) => append(a, b))

  def addOne(l: List[Int]): List[Int] = foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean) = foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil: List[B])((h, t) => append(f(h), t))

  def filter2[A](l: List[A])(f: A => Boolean) = flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addCorrespondElement(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addCorrespondElement(t1, t2))
  }

  def applyCorrespondElement[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), applyCorrespondElement(t1, t2)(f))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def f(ll: List[A], lSub: List[A]): Boolean = (ll, lSub) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) f(t1, t2) else f(t1, sub)
    }
    f(l, sub)
  }

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](tree: Tree[A])(l: A => B)(b: (B,B) => B): B = tree match {
    case Leaf(value) => l(value)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

}