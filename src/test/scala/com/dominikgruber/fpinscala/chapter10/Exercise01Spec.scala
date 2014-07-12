package com.dominikgruber.fpinscala.chapter10

import com.dominikgruber.fpinscala.chapter10.Chapter10._
import org.scalatest._

class Exercise01Spec extends FlatSpec with Matchers {

  "intAddition" should "yield correct results" in {
    import com.dominikgruber.fpinscala.chapter10.Chapter10.intAddition._

    op(1, 1) should be (2)
    op(1, -1) should be (0)
    op(1, zero) should be (1)
    op(1, op(2, 3)) should be (op(op(1, 2), 3))
    op(zero, zero) should be (zero)
  }

  "intMultiplication" should "yield correct results" in {
    import com.dominikgruber.fpinscala.chapter10.Chapter10.intMultiplication._

    op(1, 1) should be (1)
    op(2, 3) should be (6)
    op(3, zero) should be (3)
    op(2, op(3, 4)) should be (op(op(2, 3), 4))
    op(zero, zero) should be (zero)
  }

  "booleanOr" should "yield correct results" in {
    import com.dominikgruber.fpinscala.chapter10.Chapter10.booleanOr._

    op(true, true) should be (true)
    op(true, false) should be (true)
    op(false, true) should be (true)
    op(false, false) should be (false)
    op(true, zero) should be (true)
    op(false, zero) should be (false)
    op(true, op(true, false)) should be (op(op(true, true), false))
    op(zero, zero) should be (zero)
  }

  "booleanAnd" should "yield correct results" in {
    import com.dominikgruber.fpinscala.chapter10.Chapter10.booleanAnd._

    op(true, true) should be (true)
    op(true, false) should be (false)
    op(false, true) should be (false)
    op(false, false) should be (false)
    op(true, zero) should be (true)
    op(false, zero) should be (false)
    op(true, op(true, true)) should be (op(op(true, true), true))
    op(true, op(false, true)) should be (op(op(true, false), true))
    op(zero, zero) should be (zero)
  }
}
