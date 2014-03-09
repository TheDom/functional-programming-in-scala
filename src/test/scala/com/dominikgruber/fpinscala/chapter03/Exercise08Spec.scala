package com.dominikgruber.fpinscala.chapter03

import org.scalatest._

/**
 * See what happens when you pass Nil and Cons themselves to foldRight, like
 * this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)). What do you think
 * this says about the relationship between foldRight and the data constructors
 * of List?
 */
class Exercise08Spec extends FlatSpec with Matchers {

    "foldRight" should "return the same list" in {
      val l = List(1, 2, 3)
      List.foldRight(l, Nil: List[Int])(Cons(_, _)) should be (l)
    }
}