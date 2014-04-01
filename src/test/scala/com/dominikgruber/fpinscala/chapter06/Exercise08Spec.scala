package com.dominikgruber.fpinscala.chapter06

import org.scalatest._

class Exercise08Spec extends FlatSpec with Matchers {

  "nonNegativeLessThan" should "" in {
    Chapter06.nonNegativeLessThan(100)(Simple(42))._1 should be (53)
  }
}