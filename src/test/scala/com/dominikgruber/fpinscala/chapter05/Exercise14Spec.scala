package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise14Spec extends FlatSpec with Matchers {

  "startsWith" should "return true" in {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) should be (true)
  }

  it should "return false (I)" in {
    Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)) should be (false)
  }

  it should "return false (II)" in {
    Stream(1, 2, 3).startsWith(Stream(0)) should be (false)
  }
}