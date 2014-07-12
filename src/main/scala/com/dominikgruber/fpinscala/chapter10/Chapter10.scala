package com.dominikgruber.fpinscala.chapter10

import com.dominikgruber.fpinscala.chapter08.{Gen, Prop}
import com.dominikgruber.fpinscala.chapter08.Prop._

object Chapter10 {

  /**
   * Exercise 01
   * Give Monoid instances for integer addition and multiplication as well as
   * the Boolean operators.
   */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] =  new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] =  new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  /**
   * Exercise 02
   * Give a Monoid instance for combining Option values.
   */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    def zero: Option[A] = None
  }

  /**
   * Exercise 03
   * A function having the same argument and return type is sometimes called an
   * endofunction. Write a monoid for endofunctions.
   */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    def zero: A => A = (a => a)
  }

  /**
   * Exercise 04
   * Use the property-based testing framework we developed in part 2 to
   * implement a property for the monoid laws. Use your property to test the
   * monoids we’ve written.
   */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associative = forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3))

    val identity = forAll(gen)((a: A) =>
      m.op(a, m.zero) == a && m.op(m.zero, a) == a)

    associative && identity
  }


}
