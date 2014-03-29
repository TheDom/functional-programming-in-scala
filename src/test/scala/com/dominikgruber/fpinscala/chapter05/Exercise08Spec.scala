package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise08Spec extends FlatSpec with Matchers {

  "constant" should "deliver a stream of 1" in {
    Stream.constant(1).take(3).toList should be (List(1, 1, 1))
  }
}