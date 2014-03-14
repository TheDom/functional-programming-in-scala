package com.dominikgruber.fpinscala.chapter04

import org.scalatest._

class Exercise06Spec extends FlatSpec with Matchers {

  "map" should "multiply by 2" in {
    Right(1) map (_ * 2) should be (Right(2))
  }

  it should "stay Left" in {
    val e = new Exception("This is an exception")
    Left(e) map (_ => false) should be (Left(e))
  }

  "flatMap" should "multiply by 2" in {
    Right(1) flatMap (x => Right(x * 2)) should be (Right(2))
  }

  it should "stay Left" in {
    val e = new Exception("This is an exception")
    Left(e) flatMap (x => Right(false)) should be (Left(e))
  }

  "or else" should "stay Right" in {
    Right(1) orElse Right(2) should be (Right(1))
  }

  it should "change from Left to Right" in {
    Left(false) orElse Right(true) should be (Right(true))
  }

  "map2" should "multiply integers" in {
    Right(2).map2(Right(3))(_ * _) should be (Right(6))
  }

  it should "be Left" in {
    val e = new Exception("This is an exception")
    Left(e).map2(Right(3))((_, _) => false) should be (Left(e))
    Right(2).map2(Left(e))((_, _) => false) should be (Left(e))
  }
}