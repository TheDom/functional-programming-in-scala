package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise26Spec extends FlatSpec with Matchers {

  "maximum" should "work correctly (I)" in {
    val b = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.maximum(b) should be (3)
  }

  it should "work correctly (II)" in {
    val b = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
    Tree.maximum(b) should be (3)
  }
}