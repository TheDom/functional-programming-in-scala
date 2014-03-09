package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise09Spec extends FlatSpec with Matchers {

  "length" should "return 0 on an empty list" in {
    List.length(Nil) should be (0)
  }

  it should "return 5 for List(1, 2, 3, 4, 5)" in {
    List.length(List(1, 2, 3, 4, 5)) should be (5)
  }
}