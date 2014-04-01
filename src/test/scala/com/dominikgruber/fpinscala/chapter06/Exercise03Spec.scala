package com.dominikgruber.fpinscala.chapter06

import org.scalatest._

class Exercise03Spec extends FlatSpec with Matchers {

  "intDouble" should "return an int and a double" in {
    Chapter06.intDouble(Simple(42))._1 should be ((16159453, 0.5967354853637516))
  }

  "doubleInt" should "return a double and an int" in {
    Chapter06.doubleInt(Simple(42))._1 should be ((0.5967354853637516, 16159453))
  }

  "double3" should "return three doubles" in {
    Chapter06.double3(Simple(42))._1 should be ((0.007524831686168909, 0.5967354853637516, 0.15846728440374136))
  }
}