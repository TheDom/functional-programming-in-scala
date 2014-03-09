package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise18Spec extends FlatSpec with Matchers {

  "map" should "return Nil for an empty list" in {
    List.map(Nil)(x => x) should be (Nil)
  }

  it should "add 1 to each element" in {
    List.map(List(1, 2, 3))(x => x + 1) should be (List(2, 3, 4))
  }
}