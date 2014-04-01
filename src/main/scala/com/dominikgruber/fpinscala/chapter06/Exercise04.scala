package com.dominikgruber.fpinscala.chapter06

object Exercise04 {

  /**
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (Nil, rng)
    else {
      val (x, rng2) = rng.nextInt
      val (xs, rng3) = ints(count - 1)(rng2)
      (x :: xs, rng3)
    }
}