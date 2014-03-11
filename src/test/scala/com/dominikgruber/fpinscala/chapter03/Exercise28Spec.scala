package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise28Spec extends FlatSpec with Matchers {

  "map" should "add 1 to each element" in {
    val b1 = Branch(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))), Leaf(5))
    val f = (x: Int) => x + 1
    val b2 = Branch(Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(4), Leaf(5)))), Leaf(6))
    Tree.map(b1)(f) should be (b2)
  }
}