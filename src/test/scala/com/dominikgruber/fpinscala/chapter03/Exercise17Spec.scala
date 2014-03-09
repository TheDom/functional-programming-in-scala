package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise17Spec extends FlatSpec with Matchers {

  "doubleListToString" should "return Nil for an empty list" in {
    List.doubleListToString(Nil) should be (Nil)
  }

  it should "turn a list of doubles to a list of strings" in {
    List.doubleListToString(List(1.0, 2.0, 3.0)) should be (List("1.0", "2.0", "3.0"))
  }
}