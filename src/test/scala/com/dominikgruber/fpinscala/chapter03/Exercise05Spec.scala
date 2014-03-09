package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise05Spec extends FlatSpec with Matchers {

  "dropWhile" should "return Nil for an empty List" in {
    List.dropWhile(Nil, (x: Nothing) => false) should be (Nil)
  }

  it should "drop all elements < 4 on List(1, 2, 3, 4, 5) " in {
    List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 4) should be (List(4, 5))
  }
}