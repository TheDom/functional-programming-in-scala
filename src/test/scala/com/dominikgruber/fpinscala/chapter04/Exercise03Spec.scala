package com.dominikgruber.fpinscala.chapter04

import org.scalatest._

class Exercise03Spec extends FlatSpec with Matchers {

  "map2" should "multiply" in {
    Chapter04.map2(Some(2), Some(3))(_ * _) should be (Some(6))
  }

  it should "be None" in {
    val f = (x: Int, y: Int) => x * y
    Chapter04.map2(Some(2), None)(f) should be (None)
    Chapter04.map2(None, Some(3))(f) should be (None)
  }
}