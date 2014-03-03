package com.dominikgruber.fpinscala.chapter02

import org.scalatest._

class Exercise05Spec extends FlatSpec with Matchers {

  "compose" should "compose two functions (I)" in {
    val compFunc = Exercise05.compose((x: Double) => x * 2, (y: Double) => y / 2)
    compFunc(10) should be (10)
  }

  it should "compose two functions (II)" in {
    val compFunc = Exercise05.compose((x: Int) => x % 10, (y: Int) => y * 2)
    compFunc(77) should be (4)
  }
}