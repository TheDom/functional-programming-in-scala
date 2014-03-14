package com.dominikgruber.fpinscala.chapter04

import org.scalatest._

class Exercise05Spec extends FlatSpec with Matchers {

  "traverse" should "convert ints to strings" in {
      Chapter04.traverse(List(1, 2, 3))(x => Some(x.toString)) should be (Some(List("1", "2", "3")))
  }

  it should "be None" in {
    Chapter04.traverse(List(1, 2, 3))(_ => None) should be (None)
  }

  "sequence2" should "make a List" in {
    Chapter04.sequence2(List(Some(1), Some(2), Some(3))) should be (Some(List(1, 2, 3)))
  }

  it should "be None" in {
    Chapter04.sequence2(List(Some(1), None, Some(3))) should be (None)
  }
}