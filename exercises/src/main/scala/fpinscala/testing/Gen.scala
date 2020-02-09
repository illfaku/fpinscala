package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop { self =>
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = self.check && p.check
  }
}

case class GenState[A](sample: State[RNG,A]) {

  def flatMap[B](f: A => GenState[B]): GenState[B] = GenState(sample.flatMap(f(_).sample))

  def listOfN(size: GenState[Int]): GenState[List[A]] = size.flatMap(GenState.listOfN(_, this))
}

object GenState {

  def choose(start: Int, stopExclusive: Int): GenState[Int] = {
    require(start >= 0 && start < stopExclusive)
    GenState(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(start + _))
  }

  def unit[A](a: => A): GenState[A] = GenState(State.unit(a))

  def boolean: GenState[Boolean] = GenState(State[RNG, Int](_.nextInt).map(_ % 2 == 1))

  def listOfN[A](n: Int, g: GenState[A]): GenState[List[A]] = GenState(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: GenState[A], g2: GenState[A]): GenState[A] = boolean.flatMap(if (_) g1 else g2)
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

