package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise13Spec extends FlatSpec with Matchers {

  "foldLeft2" should "behave the same as foldLeft" in {
    def sum(ns: List[Int]) =
      List.foldLeft(ns, 0)(_ + _)

    def sum2(ns: List[Int]) =
      List.foldLeft2(ns, 0)(_ + _)

    val l = List(1, 2, 3, 4, 5)
    sum2(l) should be (sum(l))
  }

  "foldRight2" should "behave the same as foldRight" in {
    def length[A](l: List[A]): Int =
      List.foldRight(l, 0)((_, y) => y + 1)

    def length2[A](l: List[A]): Int =
      List.foldRight2(l, 0)((_, y) => y + 1)

    val l = List(1, 2, 3, 4, 5)
    length2(l) should be (length(l))
  }
}