package com.dominikgruber.fpinscala.chapter15

import org.scalatest.{Matchers, FlatSpec}

class ProcessSpec extends FlatSpec with Matchers {
  import Process._

  "filter" should "emit even values" in {
    val even = filter((x: Int) => x % 2 == 0)
    val evens = even(Stream(1,2,3,4)).toList
    evens should be (List(2,4))
  }

  "take" should "take three elements" in {
    take(3)(Stream(1,2,3,4,5)).toList should be (List(1,2,3))
  }

  "drop" should "drop three elements" in {
    drop(3)(Stream(1,2,3,4,5)).toList should be (List(4,5))
  }

  "takeWhile" should "take three elements" in {
    takeWhile[Int](_ != 4)(Stream(1,2,3,4,5)).toList should be (List(1,2,3))
  }

  it should "take one element" in {
    takeWhile[Int](_ == 1)(Stream(1,2,1,2,1,2)).toList should be (List(1))
  }

  "dropWhile" should "drop three elements" in {
    dropWhile[Int](_ < 4)(Stream(1,2,3,4,5)).toList should be (List(4,5))
  }

  it should "drop one element" in {
    dropWhile[Int](_ < 2)(Stream(1,2,1,2,1,2)).toList should be (List(2,1,2,1,2))
  }

  "count" should "work" in {
    count(Stream("a", "b", "c")).toList should be (List(1,2,3))
  }

  "mean" should "emit a running average" in {
    mean(Stream(1, 3, 5, 31)).toList should be (List(1, 2, 3, 10))
  }

  "sumViaLoop" should "work" in {
    sumViaLoop(Stream(1,2,3,4,5)).toList should be (List(1,3,6,10,15))
  }

  "countViaLoop" should "work" in {
    countViaLoop(Stream("a", "b", "c")).toList should be (List(1,2,3))
  }

  "|>" should "work with the example from the book" in {
    val p = filter[Int](_ % 2 == 0) |> lift(_ + 1)
    p(Stream(1,2,3,4,5)).toList should be (List(3,5))
  }
}
