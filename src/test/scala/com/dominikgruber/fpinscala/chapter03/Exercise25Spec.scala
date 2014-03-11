package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise25Spec extends FlatSpec with Matchers {

  "size" should "count correctly" in {
    val b = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.size(b) should be (5)
  }
}