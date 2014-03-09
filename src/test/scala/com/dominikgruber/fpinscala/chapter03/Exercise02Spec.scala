package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise02Spec extends FlatSpec with Matchers {

   "tail" should "return Nil for an empty list" in {
     List.tail(Nil) should be (Nil)
   }

  it should "return Nil for List(1)" in {
    List.tail(List(1)) should be (Nil)
  }

  it should "return List(2, 3) for List(1, 2, 3)" in {
    List.tail(List(1, 2, 3)) should be (List(2, 3))
  }
}