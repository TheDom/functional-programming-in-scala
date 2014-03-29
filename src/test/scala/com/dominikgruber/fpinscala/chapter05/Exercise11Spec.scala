package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise11Spec extends FlatSpec with Matchers {

  "unfold" should "generate an stream" in {
    val stream = Stream.unfold(0)((a) => Some((a, a + 1)))
    stream.take(4).toList should be (List(0, 1, 2, 3))
  }
}
