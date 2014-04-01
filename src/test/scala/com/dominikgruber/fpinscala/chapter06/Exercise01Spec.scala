package com.dominikgruber.fpinscala.chapter06

import org.scalatest._

class Exercise01Spec extends FlatSpec with Matchers {

  "nonNegativeInt" should "be a non negative integer" in {
    Chapter06.nonNegativeInt(Simple(-1))._1 should be (384749)
  }
}