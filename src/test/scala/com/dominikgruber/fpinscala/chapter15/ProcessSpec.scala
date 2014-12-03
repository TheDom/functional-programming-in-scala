package com.dominikgruber.fpinscala.chapter15

import org.scalatest.{Matchers, FlatSpec}

class ProcessSpec extends FlatSpec with Matchers {

  "filter" should "emit even values" in {
    val even = Process.filter((x: Int) => x % 2 == 0)
    val evens = even(Stream(1,2,3,4)).toList
    evens should be (List(2,4))
  }

  "take" should "take three elements" in {
    val take3 = Process.take[Int](3)
    take3(Stream(1,2,3,4,5)).toList should be (List(1,2,3))
  }

  "drop" should "drop three elements" in {
    val drop3 = Process.drop[Int](3)
    drop3(Stream(1,2,3,4,5)).toList should be (List(4,5))
  }

  "takeWhile" should "take three elements" in {
    val take3 = Process.takeWhile[Int](_ != 4)
    take3(Stream(1,2,3,4,5)).toList should be (List(1,2,3))
  }

  it should "take one element" in {
    val take1 = Process.takeWhile[Int](_ == 1)
    take1(Stream(1,2,1,2,1,2)).toList should be (List(1))
  }

  "dropWhile" should "drop three elements" in {
    val drop3 = Process.dropWhile[Int](_ < 4)
    drop3(Stream(1,2,3,4,5)).toList should be (List(4,5))
  }

  it should "drop one element" in {
    val drop1 = Process.dropWhile[Int](_ < 2)
    drop1(Stream(1,2,1,2,1,2)).toList should be (List(2,1,2,1,2))
  }

  "count" should "work" in {
    Process.count(Stream("a", "b", "c")).toList should be (List(1,2,3))
  }

  "mean" should "emit a running average" in {
    Process.mean(Stream(1, 3, 5, 31)).toList should be (List(1, 2, 3, 10))
  }
}
