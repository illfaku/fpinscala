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
}
