package com.dominikgruber.fpinscala.chapter06

object Exercise02 {

  /**
   * Write a function to generate a Double between 0 and 1, not including 1.
   * Note: you can use Int.MaxValue to obtain the maximum positive integer value
   * and you can use x.toDouble to convert an x: Int to a Double.
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = Exercise01.nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), rng2)
  }
}