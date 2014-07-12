package com.dominikgruber.fpinscala.chapter10

import com.dominikgruber.fpinscala.chapter06.Simple
import com.dominikgruber.fpinscala.chapter08.Gen
import com.dominikgruber.fpinscala.chapter10.Chapter10._
import org.scalatest._

class Exercise04Spec extends FlatSpec with Matchers {

  "monoidLaws" should "be fullfilled by Int Monoids" in {
    monoidLaws(intAddition, Gen.choose(-1000, 1000)).run(100, 100, Simple(1)) should be (None)
    monoidLaws(intMultiplication, Gen.choose(-1000, 1000)).run(100, 100, Simple(1)) should be (None)
  }
}
