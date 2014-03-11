package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise20Spec extends FlatSpec with Matchers {

  "flatMap" should "duplicate elements" in {
    val l = List(1, 2, 3)
    val f = (i: Int) => List(i, i)
    List.flatMap(l)(f) should be (List(1, 1, 2, 2, 3, 3))
  }
}