package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise03Spec extends FlatSpec with Matchers {

  "setHead" should "return List(1) if 1 gets set on Nil" in {
    List.setHead(1, Nil) should be (List(1))
  }

  it should "return List(0, 2, 3) if 0 gets set on List(1, 2, 3)" in {
    List.setHead(0, List(1, 2, 3)) should be (List(0, 2, 3))
  }
}