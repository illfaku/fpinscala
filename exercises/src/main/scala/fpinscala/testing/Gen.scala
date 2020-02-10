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

case class Prop(run: (TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop((t, r) => {
    val result = run(t, r)
    if (result.isFalsified) result
    else p.run(t, r)
  })
  def ||(p: Prop): Prop = Prop((t, r) => {
    val result = run(t, r)
    if (!result.isFalsified) result
    else p.run(t, r)
  })
}

object Prop {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    require(start >= 0 && start < stopExclusive)
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(start + _))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State[RNG, Int](_.nextInt).map(_ % 2 == 1))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val x = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double)).flatMap(d => if (d < x) g1._1 else g2._1)
  }
}

case class Gen[A](sample: State[RNG,A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_, this))

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A])

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(size => Gen.listOfN(size, g))
}
