package com.dominikgruber.fpinscala.chapter07

object Chapter07 {
  import Par._

  /**
   * Exercise 01
   * Par.map2 is a new higher-order function for combining the result of two
   * parallel computations. What is its signature? Give the most general
   * signature possible (don't assume it works only for Int).
   */
  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???
}
