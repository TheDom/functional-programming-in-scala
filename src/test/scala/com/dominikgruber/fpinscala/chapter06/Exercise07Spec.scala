package com.dominikgruber.fpinscala.chapter06

import org.scalatest._

class Exercise07Spec extends FlatSpec with Matchers {
  import Chapter06._

  "sequence" should "return a list" in {
    sequence(List(unit(1), unit(2), unit(3)))(Simple(42))._1 should be (List(1, 2, 3))
  }

  "ints2" should "return a list of random ints" in {
    ints2(5)(Simple(42))._1 should be (List(16159453, -1281479697, -340305902, -2015756020, 1770001318))
  }
}