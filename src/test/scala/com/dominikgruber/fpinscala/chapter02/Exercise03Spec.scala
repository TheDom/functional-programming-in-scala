package com.dominikgruber.fpinscala.chapter02

import org.scalatest._

class Exercise03Spec extends FlatSpec with Matchers {

  "curry" should "multiply" in {
    val curryFunc = Exercise03.curry((x: Int, y: Int) => x * y)
    curryFunc(2)(2000) should be (4000)
  }
}