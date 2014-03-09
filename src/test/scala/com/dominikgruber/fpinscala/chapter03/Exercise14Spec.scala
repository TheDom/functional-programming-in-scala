package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise14Spec extends FlatSpec with Matchers {

  "append2" should "return Nil on two empty lists" in {
    List.append2(Nil, Nil) should be (Nil)
  }

  it should "return List(1, 2, 3, 4, 5) for List(1, 2) and List (3, 4, 5)" in {
    List.append2(List(1, 2), List(3, 4, 5)) should be (List(1, 2, 3, 4, 5))
  }
}