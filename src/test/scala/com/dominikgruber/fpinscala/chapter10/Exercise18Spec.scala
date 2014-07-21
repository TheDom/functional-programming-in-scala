package com.dominikgruber.fpinscala.chapter10

import org.scalatest._
import com.dominikgruber.fpinscala.chapter10.Chapter10._

class Exercise18Spec extends FlatSpec with Matchers {

  "bag" should "count correctly" in {
    bag(Vector("a", "rose", "is", "a", "rose")) should be (Map("a" -> 2, "rose" -> 2, "is" -> 1))
  }
}
