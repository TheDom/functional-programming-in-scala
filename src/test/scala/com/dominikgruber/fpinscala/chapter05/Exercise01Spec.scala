package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise01Spec extends FlatSpec with Matchers {

  "toList" should "convert a Stream to a List" in {
    Stream(1, 2, 3).toList should be (List(1, 2, 3))
  }

  it should "work with an empty Stream" in {
    Stream().toList should be (List())
  }
}