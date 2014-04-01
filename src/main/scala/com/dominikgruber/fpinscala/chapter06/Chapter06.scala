package com.dominikgruber.fpinscala.chapter06

object Chapter06 {

  /**
   * Exercise 1
   * Write a function that uses RNG.nextInt to generate a random integer between
   * 0 and Int.maxValue (inclusive). Make sure to handle the corner case when
   * nextInt returns Int.MinValue, which doesn't have a non-negative
   * counterpart.
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rnd) = rng.nextInt
    if (i > 0) (i, rnd)
    else if (i == Int.MinValue) (-(i + 1), rnd)
    else (-i, rnd)
  }

  /**
   * Exercise 2
   * Write a function to generate a Double between 0 and 1, not including 1.
   * Note: you can use Int.MaxValue to obtain the maximum positive integer value
   * and you can use x.toDouble to convert an x: Int to a Double.
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), rng2)
  }

  /**
   * Exercise 3
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair,
   * and a (Double, Double, Double) 3-tuple. You should be able to reuse the
   * functions you've already written.
   */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  /**
   * Exercise 4
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (Nil, rng)
    else {
      val (x, rng2) = rng.nextInt
      val (xs, rng3) = ints(count - 1)(rng2)
      (x :: xs, rng3)
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

  /**
   * Exercise 5
   * Use map to reimplement double in a more elegant way.
   */
  def double2: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /**
   * Exercise 6
   * Write the implementation of map2, based on the following signature. This
   * function takes two actions, ra and rb, a function f for combining their
   * results, and returns a new action that combines them.
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  /**
   * Exercise 7 (hard)
   * If we can combine two RNG transitions, we should be able to combine a
   * whole list of them. Implement sequence, for combining a List of transitions
   * into a single transition. Use it to reimplement the ints function you wrote
   * before. For the latter, you can use the standard library function
   * List.fill(n)(x) to make a list with x repeated n times.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())) { (f, acc) =>
      map2(f, acc)(_ :: _)
    }

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  /**
   * Exercise 8
   * Implement flatMap, then use it to implement nonNegativeLessThan.
   */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
}