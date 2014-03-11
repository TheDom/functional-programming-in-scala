package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

class Exercise24Spec extends FlatSpec with Matchers {

  "hasSubsequence" should "detect a subsequence (I)" in {
    val l1 = List(1, 1, 1, 2, 3, 4, 5)
    val l2 = List(1, 2, 3)
    List.hasSubsequence(l1, l2) should be (true)
  }

  it should "detect a subsequence (II)" in {
    val l1 = List(1, 1, 2)
    val l2 = List(1, 2)
    List.hasSubsequence(l1, l2) should be (true)
  }

  it should "detect a subsequence (III)" in {
    val l1 = List(2, 1, 2, 1, 2, 3)
    val l2 = List(2, 1, 2, 3)
    List.hasSubsequence(l1, l2) should be (true)
  }

  it should "not a detect a subsequence" in {
    val l1 = List(1, 1, 1, 2, 3, 4, 5)
    val l2 = List(1, 2, 3, 5)
    List.hasSubsequence(l1, l2) should be (false)
  }
}