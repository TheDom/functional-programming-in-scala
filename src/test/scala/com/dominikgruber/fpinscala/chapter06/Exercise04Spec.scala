package com.dominikgruber.fpinscala.chapter06

import org.scalatest._

class Exercise04Spec extends FlatSpec with Matchers {

  "ints" should "return a list of random ints" in {
    Exercise04.ints(5)(Simple(42))._1 should be (List(16159453, -1281479697, -340305902, -2015756020, 1770001318))
  }
}