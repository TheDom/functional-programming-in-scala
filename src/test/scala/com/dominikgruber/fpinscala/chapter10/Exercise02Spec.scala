package com.dominikgruber.fpinscala.chapter10

import com.dominikgruber.fpinscala.chapter10.Chapter10._
import org.scalatest._

class Exercise02Spec extends FlatSpec with Matchers {

  "optionMonoid" should "yield correct results for Int" in {
    optionMonoid.op(Some(1), Some(2)) should be (Some(1))
    optionMonoid.op(Some(1), None) should be (Some(1))
    optionMonoid.op(None, Some(2)) should be (Some(2))
    optionMonoid.op(None, None) should be (None)
    optionMonoid.op(Some(1), optionMonoid.op(Some(2), Some(3))) should be (optionMonoid.op(optionMonoid.op(Some(1), Some(2)), Some(3)))
    optionMonoid.op(None, optionMonoid.op(Some(2), Some(3))) should be (optionMonoid.op(optionMonoid.op(None, Some(2)), Some(3)))
    optionMonoid.op(Some(1), optionMonoid.op(None, Some(3))) should be (optionMonoid.op(optionMonoid.op(Some(1), None), Some(3)))
    optionMonoid.op(Some(1), optionMonoid.op(Some(2), None)) should be (optionMonoid.op(optionMonoid.op(Some(1), Some(2)), None))
    optionMonoid.op(optionMonoid.zero, optionMonoid.zero) should be (optionMonoid.zero)
  }
}
