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
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // This will match and return 3
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


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(_, tail) => Cons(h, tail)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n > 0) {
      l match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }
    } else l
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(h, tail) =>
      if (f(h)) dropWhile(tail, f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, c) => c + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(h, z))(f)
  }

  def sumLeft(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def productLeft(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((_, c) => c + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])(Cons(_, _))

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

  def zipWithInt(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (t, Nil) => t
    case (Nil, t) => t
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipWithInt(t1, t2))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @annotation.tailrec
  def startsWith[A](list: List[A], prefix: List[A]): Boolean = (list, prefix) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if (startsWith(sup, sub)) true else sup match {
      case Cons(_, tail) => hasSubsequence(tail, sub)
      case _ => false
    }
  }


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
    println(s"hasSubsequence: ${hasSubsequence(List(1, 5, 2, 6), List(5, 2))}")
  }
}
