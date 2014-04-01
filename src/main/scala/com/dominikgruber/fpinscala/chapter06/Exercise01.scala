package com.dominikgruber.fpinscala.chapter06

object Exercise01 {

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
}