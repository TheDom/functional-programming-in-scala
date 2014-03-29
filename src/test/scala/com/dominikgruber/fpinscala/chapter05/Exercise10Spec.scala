package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise10Spec extends FlatSpec with Matchers {

  "fibs" should "generate a stream of Fibonacci numbers" in {
    Stream.fibs.take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
  }
}
