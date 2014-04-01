package com.dominikgruber.fpinscala.chapter06

import org.scalatest._

class Exercise06Spec extends FlatSpec with Matchers {

  "randIntDouble" should "return an int and a double" in {
    Chapter06.randIntDouble(Simple(42))._1 should be ((16159453, 0.5967354853637516))
  }

  "randDoubleInt" should "return a double and an int" in {
    Chapter06.randDoubleInt(Simple(42))._1 should be ((0.007524831686168909, -1281479697))
  }
}