package com.dominikgruber.fpinscala.chapter04

import org.scalatest._

class Exercise07Spec extends FlatSpec with Matchers {

  "sequence" should "make a List" in {
    Either.sequence(List(Right(1), Right(2), Right(3))) should be (Right(List(1, 2, 3)))
  }

  it should "be None" in {
    Either.sequence(List(Right(1), Left(false), Right(3))) should be (Left(false))
  }

  "traverse" should "convert ints to strings" in {
    Either.traverse(List(1, 2, 3))(x => Right(x.toString)) should be (Right(List("1", "2", "3")))
  }

  it should "be None" in {
    Either.traverse(List(1, 2, 3))(_ => Left(false)) should be (Left(false))
  }
}