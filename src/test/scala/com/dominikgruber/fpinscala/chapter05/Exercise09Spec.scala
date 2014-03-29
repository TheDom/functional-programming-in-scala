package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise09Spec extends FlatSpec with Matchers {

  "from" should "generate an infinite stream" in {
    Stream.from(1).take(4).toList should be (List(1, 2, 3, 4))
  }
}
