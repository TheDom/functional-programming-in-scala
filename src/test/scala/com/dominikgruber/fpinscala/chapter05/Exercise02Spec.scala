package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise02Spec extends FlatSpec with Matchers {

  "take" should "take a few elements" in {
    Stream(1, 2, 3).take(2).toList should be (List(1, 2))
  }

  it should "work with an empty Stream" in {
    Stream().take(3).toList should be (List())
  }

  "drop" should "drop a few elements" in {
    Stream(1, 2, 3).drop(2).toList should be (List(3))
  }

  it should "work with an empty Stream" in {
    Stream().drop(3).toList should be (List())
  }
}