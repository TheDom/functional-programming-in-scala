package com.dominikgruber.fpinscala.chapter04

import org.scalatest._

class Exercise04Spec extends FlatSpec with Matchers {

  "sequency" should "make a List" in {
    Chapter04.sequence(List(Some(1), Some(2), Some(3))) should be (Some(List(1, 2, 3)))
  }

  it should "be None" in {
    Chapter04.sequence(List(Some(1), None, Some(3))) should be (None)
  }
}