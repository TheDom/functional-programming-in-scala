package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise16Spec extends FlatSpec with Matchers {

  "add1" should "return Nil for an empty list" in {
    List.add1(Nil) should be (Nil)
  }

  it should "return List(2, 3, 4, 5, 6) for List(1, 2, 3, 4, 5)" in {
    List.add1(List(1, 2, 3, 4, 5)) should be (List(2, 3, 4, 5, 6))
  }
}