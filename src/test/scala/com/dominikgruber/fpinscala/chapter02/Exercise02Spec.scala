package com.dominikgruber.fpinscala.chapter02

import org.scalatest._

class Exercise02Spec extends FlatSpec with Matchers {

  val gtFunc = (x: Int, y: Int) => y > x

  "isSorted" should "detect that an array is sorted" in {
    Exercise02.isSorted(Array(1, 2, 3, 4, 5), gtFunc) should be (true)
  }

  it should "detect that an array is not sorted" in {
    Exercise02.isSorted(Array(1, 3, 2, 5, 4), gtFunc) should be (false)
  }

  it should "detect that an array with one element is sorted" in {
    Exercise02.isSorted(Array(1), gtFunc) should be (true)
  }
}