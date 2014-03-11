package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise27Spec extends FlatSpec with Matchers {

  "depth" should "work correctly (I)" in {
    val b = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.depth(b) should be (2)
  }

  it should "work correctly (II)" in {
    val b = Branch(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))), Leaf(5))
    Tree.depth(b) should be (4)
  }

  it should "work correctly (III)" in {
    val b = Leaf(0)
    Tree.depth(b) should be (0)
  }
}