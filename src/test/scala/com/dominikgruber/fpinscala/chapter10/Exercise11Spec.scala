package com.dominikgruber.fpinscala.chapter10

import org.scalatest._

class Exercise11Spec extends FlatSpec with Matchers {

  "countWords" should "count correctly, I" in {
    WC.countWords("") should be (0)
  }

  it should "count correctly, II" in {
    WC.countWords("lorem ipsum dolor sit amet, ") should be (5)
  }
}
