package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise06Spec extends FlatSpec with Matchers {

  "init" should "return Nil for an empty List" in {
    List.init(Nil) should be (Nil)
  }

  it should "return Nil for List(1)" in {
    List.init(List(1)) should be (Nil)
  }

  it should "return List(1, 2, 3) for List(1, 2, 3, 4)" in {
    List.init(List(1, 2, 3, 4)) should be (List(1, 2, 3))
  }
}