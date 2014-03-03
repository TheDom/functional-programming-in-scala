package com.dominikgruber.fpinscala.chapter02

/**
 * Implement the higher-order function that composes
 * two functions.
 */
object Exercise05 {

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
    // a => f(g(a))
}