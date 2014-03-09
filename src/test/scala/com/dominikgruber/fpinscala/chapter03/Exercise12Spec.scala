package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise12Spec extends FlatSpec with Matchers {

  "reverse" should "return Nil on Nil" in {
    List.reverse(Nil) should be (Nil)
  }

  it should "return List(1) for List(1)" in {
    List.reverse(List(1)) should be (List(1))
  }

  it should "return List(3, 2, 1) for List(1, 2, 3)" in {
    List.reverse(List(1, 2, 3)) should be (List(3, 2, 1))
  }
}