package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

/**
 * Tests exercise 10 + 11
 */
class Exercise11Spec extends FlatSpec with Matchers {

  "sum3" should "return 0 on an empty list" in {
    List.sum3(Nil) should be (0)
  }

  it should "return 6 for List(1, 2, 3)" in {
    List.sum3(List(1, 2, 3)) should be (6)
  }

  "product3" should "return 1 on an empty list" in {
    List.product3(Nil) should be (1.0)
  }

  it should "return 6 for List(1, 2, 3)" in {
    List.product3(List(1.0, 2.0, 3.0)) should be (6.0)
  }

  it should "return 0 for List(1, 2, 3, 0)" in {
    List.product3(List(1.0, 2.0, 3.0, 0.0)) should be (0.0)
  }

  "length2" should "return 0 on an empty list" in {
    List.length2(Nil) should be (0)
  }

  it should "return 5 for List(1, 2, 3, 4, 5)" in {
    List.length2(List(1, 2, 3, 4, 5)) should be (5)
  }
}