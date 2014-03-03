package com.dominikgruber.fpinscala.chapter02

import org.scalatest._

class Exercise04Spec extends FlatSpec with Matchers {

  "uncurry" should "multiply" in {
    val uncurryFunc = Exercise04.uncurry((a: Int) => ((b: Int) => a * b))
    uncurryFunc(2, 2000) should be (4000)
  }
}