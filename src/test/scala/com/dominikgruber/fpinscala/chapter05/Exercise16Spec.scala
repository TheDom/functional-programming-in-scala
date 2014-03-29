package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise16Spec extends FlatSpec with Matchers {

  "scanRight" should "" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList should be (List(6, 5, 3, 0))
  }
}