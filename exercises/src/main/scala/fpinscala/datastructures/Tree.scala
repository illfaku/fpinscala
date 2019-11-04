package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size(t: Tree[_]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def size2(t: Tree[_]): Int = {
    @annotation.tailrec
    def loop(branches: List[Tree[_]], result: Int): Int = branches match {
      case Nil => result
      case Cons(Leaf(_), tail) => loop(tail, result + 1)
      case Cons(Branch(l, r), tail) => loop(Cons(l, Cons(r, tail)), result + 1)
    }
    loop(List(t), 0)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def maximum2(t: Tree[Int]): Int = {
    @annotation.tailrec
    def loop(branches: List[Tree[Int]], result: Int): Int = branches match {
      case Nil => result
      case Cons(Leaf(value), tail) => loop(tail, result max value)
      case Cons(Branch(l, r), tail) => loop(Cons(l, Cons(r, tail)), result)
    }
    loop(List(t), Int.MinValue)
  }

  def depth(t: Tree[_]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def depth2(t: Tree[_]): Int = {
    @annotation.tailrec
    def loop(branches: List[(Tree[_], Int)], result: Int): Int = branches match {
      case Nil => result
      case Cons((Leaf(_), d), tail) => loop(tail, result max (d + 1))
      case Cons((Branch(l, r), d), tail) => loop(Cons((l, d + 1), Cons((r, d + 1), tail)), result)
    }
    loop(List((t, 0)), 0)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}
