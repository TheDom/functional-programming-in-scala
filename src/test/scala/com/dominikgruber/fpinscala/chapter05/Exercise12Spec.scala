package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise12Spec extends FlatSpec with Matchers {

  "fibs2" should "generate a stream of Fibonacci numbers" in {
    Stream.fibs2.take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
  }

  "from2" should "generate an infinite stream" in {
    Stream.from2(1).take(4).toList should be (List(1, 2, 3, 4))
  }

  "constant2" should "deliver a stream of 1" in {
    Stream.constant2(1).take(3).toList should be (List(1, 1, 1))
  }

  "ones2" should "produce a stream of 1" in {
    Stream.ones2.take(5).toList should be (List(1, 1, 1, 1, 1))
  }
}