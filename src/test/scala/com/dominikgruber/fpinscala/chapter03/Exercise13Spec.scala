package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise13Spec extends FlatSpec with Matchers {

  "foldLeft2" should "behave the same as foldLeft" in {
    def div(ns: List[Double]) =
      List.foldLeft(ns, 1.0)(_ / _)

    def div2(ns: List[Double]) =
      List.foldLeft2(ns, 1.0)(_ / _)

    val l = List(1.0, 2.0, 3.0, 4.0, 5.0)
    div2(l) should be (div(l))
    div2(l) should be (1.0 / 1.0 / 2.0 / 3.0 / 4.0 / 5.0)
  }

  "foldRight2" should "behave the same as foldRight" in {
    def div(l: List[Double]) =
      List.foldRight(l, 1.0)(_ / _)

    def div2(l: List[Double]) =
      List.foldRight2(l, 1.0)(_ / _)

    val l = List(1.0, 2.0, 3.0, 4.0, 5.0)
    div2(l) should be (div(l))
    div2(l) should be (1.0 / (2.0 / (3.0 / (4.0 / (5.0 / 1.0)))))
  }
}