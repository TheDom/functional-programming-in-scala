package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise15Spec extends FlatSpec with Matchers {

  "concatListOfLists" should "return List(1, 2, 3, 4, 5) for List(List(1, 2), List(3, 4), List(5))" in {
    val l = List(List(1, 2), List(3, 4), List(5))
    List.concatListOfLists(l) should be (List(1, 2, 3, 4, 5))
  }
}