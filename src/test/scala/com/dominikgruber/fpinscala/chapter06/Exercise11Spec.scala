package com.dominikgruber.fpinscala.chapter06

import org.scalatest._

class Exercise11Spec extends FlatSpec with Matchers {

  "simulateMachine" should "return the expected output" in {
    val m = Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    Candy.simulateMachine(inputs).run(m)._1 should be ((14, 1))
  }
}