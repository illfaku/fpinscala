package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = ???

  def setHead[A](l: List[A], h: A): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] = ???

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = ???

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(h, z))(f)
  }

  def foldLeft2[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldRight(l, (x: B) => x)((a, b) => x => b(f(a, x)))(z)

  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, (x: B) => x)((a, b) => x => b(f(a, x)))(z)

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def append3[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a1, (x: List[A]) => x)((a, g) => l => g(Cons(a, l)))(a2)

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def increment(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def double2string(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil: List[B])((a, b) => append(f(a), b))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)


  def mkString[A](l: List[A], separator: String = ""): String = {
    foldLeft(l, new StringBuilder) { (a, b) =>
      if (b.nonEmpty) b ++= separator
      b ++= a.toString
    }.toString
  }

  def main(args: Array[String]): Unit = {
    println(s"append2: ${mkString(append2(List(1, 2, 3), List(4, 5, 6)), ", ")}")
    println(s"append3: ${mkString(append3(List(1, 2, 3), List(4, 5, 6)), ", ")}")
    println(s"flatMap: ${mkString(flatMap(List(1, 2, 3))(i => List(i, i)), ", ")}")
  }
}
