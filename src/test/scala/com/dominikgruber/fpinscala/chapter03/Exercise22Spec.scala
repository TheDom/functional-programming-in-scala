package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise22Spec extends FlatSpec with Matchers {

  "addInt" should "add elements of a list" in {
    List.addInt(List(1, 2, 3), List(4, 5, 6)) should be (List(5, 7, 9))
  }
}