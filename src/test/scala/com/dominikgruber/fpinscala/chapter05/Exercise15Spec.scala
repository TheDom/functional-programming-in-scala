package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise15Spec extends FlatSpec with Matchers {

  "tails" should "work" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) should be (List(List(1,2,3), List(2,3), List(3), Nil))
  }

  it should "work for an empty Stream" in {
    Stream().tails.toList.map(_.toList) should be (List(Nil))
  }

  "hasSubsequence" should "detect a subsequence (I)" in {
    val s1 = Stream(1, 1, 1, 2, 3, 4, 5)
    val s2 = Stream(1, 2, 3)
    s1.hasSubsequence(s2) should be (true)
  }

  it should "detect a subsequence (II)" in {
    val s1 = Stream(1, 1, 2)
    val s2 = Stream(1, 2)
    s1.hasSubsequence(s2) should be (true)
  }

  it should "detect a subsequence (III)" in {
    val s1 = Stream(2, 1, 2, 1, 2, 3)
    val s2 = Stream(2, 1, 2, 3)
    s1.hasSubsequence(s2) should be (true)
  }

  it should "not a detect a subsequence" in {
    val s1 = Stream(1, 1, 1, 2, 3, 4, 5)
    val s2 = Stream(1, 2, 3, 5)
    s1.hasSubsequence(s2) should be (false)
  }
}