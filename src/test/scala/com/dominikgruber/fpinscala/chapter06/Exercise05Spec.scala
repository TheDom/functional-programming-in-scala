package com.dominikgruber.fpinscala.chapter06

import org.scalatest._

class Exercise05Spec extends FlatSpec with Matchers {

  "double2" should "generate a double" in {
    Chapter06.double2(Simple(-10000000))._1 should be (0.3733188882470131)
  }
}