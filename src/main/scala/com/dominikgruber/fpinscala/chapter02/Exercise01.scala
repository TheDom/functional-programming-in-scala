package com.dominikgruber.fpinscala.chapter02

/**
 * Write a recursive function to get the nth Fibonacci number.
 * The first two Fibonacci numbers are 0 and 1. The nth number is
 * always the sum of the previous two–the sequence begins 0, 1, 1, 2,
 * 3, 5. Your definition should use a local tail-recursive function.
 */
object Exercise01 {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else go(n - 1, cur, prev + cur)

    go(n, 0, 1)
  }
}