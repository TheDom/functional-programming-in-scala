package com.dominikgruber.fpinscala.chapter06

object Exercise03 {

  /**
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair,
   * and a (Double, Double, Double) 3-tuple. You should be able to reuse the
   * functions you've already written.
   */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = Exercise02.double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = Exercise02.double(rng)
    val (d2, rng3) = Exercise02.double(rng2)
    val (d3, rng4) = Exercise02.double(rng3)
    ((d1, d2, d3), rng4)
  }
}