package com.dominikgruber.fpinscala.chapter04

import org.scalatest._

class Exercise02Spec extends FlatSpec with Matchers {

  "variance" should "multiply by 2" in {
    Chapter04.variance(Seq(1.0, 2.0, 3.0)) should be (Some(2.0 / 3.0))
  }

  it should "be None" in {
    Chapter04.variance(Seq[Double]()) should be (None)
  }
}