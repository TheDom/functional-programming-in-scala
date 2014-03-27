package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise06Spec extends FlatSpec with Matchers {

  "headOption2" should "return the head" in {
    Stream(1, 2, 3).headOption2 should be (Some(1))
  }

  it should "be None" in {
    Stream().headOption2 should be (None)
  }
}