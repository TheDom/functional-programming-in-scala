package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise03Spec extends FlatSpec with Matchers {

  "takeWhile" should "take a few elements" in {
    Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList should be (List(1, 2))
  }
}