import Chapter5._

val ones: Stream[Int] = Stream.cons(1, ones)
ones.take(5).toList
ones.exists(_ % 2 != 0)

ones.map(_ + 1).exists(_ % 2 == 0)
ones.takeWhile(_ == 1)
ones.forAll(_ != 1)
