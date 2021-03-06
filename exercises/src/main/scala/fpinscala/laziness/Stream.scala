package fpinscala.laziness

import scala.collection.mutable.ListBuffer
import scala.{Stream => _}

trait Stream[+A] {

  def toList0: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList0
  }

  def toList: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], result: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: result)
      case _ => result.reverse
    }
    loop(this, Nil)
  }

  def toList1: List[A] = {
    val buffer = ListBuffer.empty[A]
    @annotation.tailrec
    def loop(s: Stream[A]): List[A] = s match {
      case Cons(h, t) => buffer += h(); loop(t())
      case _ => buffer.toList
    }
    loop(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Stream.empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, s) => if (p(a)) Stream.cons(a, s) else Stream.empty)
  }

  def headOption: Option[A] = foldRight(Option.empty[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)(Stream.cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((h, t) => f(h).append(t))

  def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWithUnfold[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, that)) {
    case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
    case _ => None
  }

  def zipAllUnfold[B](that: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, that)) {
    case (Cons(ah, at), Cons(bh, bt)) => Some(((Some(ah()), Some(bh())), (at(), bt())))
    case (Cons(ah, at), empty) => Some(((Some(ah()), None), (at(), empty)))
    case (empty, Cons(bh, bt)) => Some(((None, Some(bh())), (empty, bt())))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAllUnfold(s).takeWhile {
    case (_, b) => b.isDefined
  }.forAll {
    case (a, b) => a == b
  }

  def tails: Stream[Stream[A]] = Stream.cons(this, Stream.unfold(this) {
    case Cons(_, t) => Some(t() -> t())
    case _ => None
  })

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Cons(h, t) =>
      lazy val s @ Cons(next, _) = t().scanRight(z)(f)
      Stream.cons(f(h(), next()), s)
    case _ => Stream(z)
  }

  def scanRight2[B](z: => B)(f: (A, => B) => B): Stream[B] = foldRight(Stream(z)) { (a, s) =>
    lazy val Cons(next, _) = s
    Stream.cons(f(a, next()), s)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).scanRight2(0)(_ + _).toList)
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def f(n: Int, m: Int): Stream[Int] = cons(m, f(m, n + m))
    f(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map(x => cons(x._1, unfold(x._2)(f))).getOrElse(empty)
  }

  val onesUnfold: Stream[Int] = unfold(1)(s => Some((s, s)))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  val fibsUnfold: Stream[Int] = unfold((0: Int, 1: Int)) {
    case (n, m) => Some((n, (m, n + m)))
  }
}
