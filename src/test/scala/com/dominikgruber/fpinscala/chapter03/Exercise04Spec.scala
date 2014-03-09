package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise04Spec extends FlatSpec with Matchers {

  "drop" should "return Nil for an empty List" in {
    List.drop(10, Nil) should be (Nil)
  }

  it should "return List(5) for drop(4, List(1, 2, 3, 4, 5)" in {
    List.drop(4, List(1, 2, 3, 4, 5)) should be (List(5))
  }

  it should "return the same List if nothing gets dropped" in {
    List.drop(0, List(1, 2, 3, 4, 5)) should be (List(1, 2, 3, 4, 5))
  }
}