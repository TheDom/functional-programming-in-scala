package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise29Spec extends FlatSpec with Matchers {

  "size" should "count correctly" in {
    val b = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.size2(b) should be (Tree.size(b))
  }

  "maximum" should "work correctly (I)" in {
    val b = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.maximum2(b) should be (Tree.maximum(b))
  }

  it should "work correctly (II)" in {
    val b = Branch(Branch(Leaf(1), Leaf(3)), Leaf(2))
    Tree.maximum2(b) should be (Tree.maximum(b))
  }

  "depth" should "work correctly (I)" in {
    val b = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.depth2(b) should be (Tree.depth(b))
  }

  it should "work correctly (II)" in {
    val b = Branch(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))), Leaf(5))
    Tree.depth2(b) should be (Tree.depth(b))
  }

  it should "work correctly (III)" in {
    val b = Leaf(0)
    Tree.depth2(b) should be (Tree.depth(b))
  }

  "map" should "add 1 to each element" in {
    val b1 = Branch(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))), Leaf(5))
    val f = (x: Int) => x + 1
    Tree.map2(b1)(f) should be (Tree.map(b1)(f))
  }
}