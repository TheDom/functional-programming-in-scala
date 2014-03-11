package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise21Spec extends FlatSpec with Matchers {

  "filter2" should "remove odd numbers from a list" in {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    List.filter2(l)(x => x % 2 == 0) should be (List(2, 4, 6, 8, 10))
  }

}