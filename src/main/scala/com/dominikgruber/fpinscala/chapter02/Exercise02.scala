package com.dominikgruber.fpinscala.chapter02

/**
 * Implement isSorted, which checks whether an Array[A] is sorted
 * according to a given comparison function.
 */
object Exercise02 {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (gt(as(n - 1), as(n))) loop(n + 1)
      else false
    }
    loop(1)
  }
}