package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise13Spec extends FlatSpec with Matchers {

  "map2" should "transform an int Stream to strings" in {
    Stream(1, 2, 3).map2(_.toString).toList should be (List("1", "2", "3"))
  }

  "take2" should "take a few elements" in {
    Stream(1, 2, 3).take2(2).toList should be (List(1, 2))
  }

  it should "work with an empty Stream" in {
    Stream().take2(3).toList should be (List())
  }

  "takeWhile3" should "take a few elements" in {
    Stream(1, 2, 3, 4, 5).takeWhile3(_ < 3).toList should be (List(1, 2))
  }

  it should "not take anything" in {
    Stream(1, 2, 3).takeWhile3(_ > 10).toList should be (Nil)
  }

  "zipWith" should "add two streams" in {
    val s1 = Stream.from(1)
    val s2 = Stream.from(10)
    s1.zipWith(s2)(_ + _).take(3).toList should be (List(11, 13, 15))
  }

  "zipAll" should "work on infinite streams" in {
    val s1 = Stream.from(1)
    val s2 = Stream.from(10)
    s1.zipAll(s2).take(3).toList should be (List((Some(1), Some(10)), (Some(2), Some(11)), (Some(3), Some(12))))
  }

  it should "work if stream 1 ends" in {
    val s1 = Stream(1)
    val s2 = Stream.from(10)
    s1.zipAll(s2).take(3).toList should be (List((Some(1), Some(10)), (None, Some(11)), (None, Some(12))))
  }

  it should "work if stream 2 ends" in {
    val s1 = Stream.from(1)
    val s2 = Stream(10)
    s1.zipAll(s2).take(3).toList should be (List((Some(1), Some(10)), (Some(2), None), (Some(3), None)))
  }

  it should "work with empty streams" in {
    Stream().zipAll(Stream()).take(3).toList should be (Nil)
  }
}