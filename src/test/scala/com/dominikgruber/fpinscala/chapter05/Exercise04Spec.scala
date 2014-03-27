package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise04Spec extends FlatSpec with Matchers {

  "forAll" should "succeed" in {
    Stream(1, 2, 3, 4, 5).forAll(_ < 10) should be (true)
  }

  it should "fail" in {
    Stream(1, 2, 3, 4, 5).forAll(_ > 10) should be (false)
  }
}