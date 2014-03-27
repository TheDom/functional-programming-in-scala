package com.dominikgruber.fpinscala.chapter05

import org.scalatest._

class Exercise07Spec extends FlatSpec with Matchers {

  "map" should "transform an int Stream to strings" in {
    Stream(1, 2, 3).map(_.toString).toList should be (List("1", "2", "3"))
  }

  "filter" should "filter out odd numbers" in {
    Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList should be (List(2, 4))
  }

  "append" should "append a Stream" in {
    Stream(1, 2).append(Stream(3, 4)).toList should be (List(1, 2, 3, 4))
  }

  "flatMap" should "duplicate the elements" in {
    Stream(1, 2, 3).flatMap((x) => Stream(x, x)).toList should be (List(1, 1, 2, 2, 3, 3))
  }
}