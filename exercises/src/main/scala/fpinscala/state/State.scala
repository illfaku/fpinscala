package fpinscala.state

import scala.collection.mutable.ListBuffer


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def main(args: Array[String]): Unit = {

  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (result, nextRng) = rng.nextInt
    (if (result == Int.MinValue) 0 else result.abs, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (result, nextRng) = nonNegativeInt(rng)
    (if (result == Int.MaxValue) 0 else result.toDouble / Int.MaxValue, nextRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (result1, nextRng1) = rng.nextInt
    val (result2, nextRng2) = double(nextRng1)
    ((result1, result2), nextRng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (result1, nextRng1) = double(rng)
    val (result2, nextRng2) = nextRng1.nextInt
    ((result1, result2), nextRng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (result1, nextRng1) = double(rng)
    val (result2, nextRng2) = double(nextRng1)
    val (result3, nextRng3) = double(nextRng2)
    ((result1, result2, result3), nextRng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(n: Int, r: RNG, result: List[Int]): (List[Int], RNG) = {
      if (n > 0) {
        val (x, rn) = rng.nextInt
        loop(n - 1, rn, x :: result)
      } else {
        (result, r)
      }
    }
    loop(count, rng, Nil)
  }

  val doubleRand: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case h :: t => map2(h, sequence(t))(_ :: _)
    case _ => unit(Nil)
  }

  def sequenceTail[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    val buffer = new ListBuffer[A]
    @annotation.tailrec
    def loop(s: List[Rand[A]], r: RNG): (List[A], RNG) = s match {
      case h :: t =>
        val (a, rn) = h(r)
        buffer += a
        loop(t, rn)
      case Nil => (buffer.toList, r)
    }
    loop(fs, rng)
  }

  def sequenceFold[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(Nil: List[A]))(map2(_, _)(_ :: _))

  def intSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(_.nextInt))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(f.andThen(unit))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(f(a, _)))
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
