package com.dominikgruber.fpinscala.chapter04

import org.scalatest._

class Exercise01Spec extends FlatSpec with Matchers {

  "map" should "multiply by 2" in {
    Some(2).map(_ * 2) should be (Some(4))
  }

  it should "stay None" in {
    None.map(x => x) should be (None)
  }

  "flatMap" should "multiply by 2" in {
    Some(2).map(x => Some(x * 2)) should be (Some(Some(4)))
  }

  it should "stay None" in {
    None.map(x => x) should be (None)
  }

  "getOrElse" should "get" in {
    Some(2).getOrElse(0) should be (2)
  }

  it should "else" in {
    None.getOrElse(0) should be (0)
  }

  "orElse" should "return Option" in {
    Some(2).orElse(Some(3)) should be (Some(2))
  }

  it should "else" in {
    None.orElse(Some(3)) should be (Some(3))
  }

  "filter" should "keep value" in {
    Some(2).filter(_ % 2 == 0) should be (Some(2))
  }

  it should "not keep value" in {
    Some(2).filter(_ % 2 != 0) should be (None)
  }

  it should "stay None" in {
    None.filter(_ => true) should be (None)
    None.filter(_ => false) should be (None)
  }
}