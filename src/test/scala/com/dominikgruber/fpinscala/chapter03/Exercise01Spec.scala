package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise01Spec extends FlatSpec with Matchers {

  "Given pattern match" should "result in 3" in {
    Exercise01.patternmatch should be (3)
  }
}