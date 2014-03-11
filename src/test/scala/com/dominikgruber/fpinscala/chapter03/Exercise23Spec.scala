package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise23Spec extends FlatSpec with Matchers {

  "zip" should "subtract elements of list 2 from list 1" in {
    val l1 = List(4, 5, 6)
    val l2 = List(1, 2, 3)
    val f = (a: Int, b: Int) => a - b
    List.zip(l1, l2)(f) should be (List(3, 3, 3))
  }
}