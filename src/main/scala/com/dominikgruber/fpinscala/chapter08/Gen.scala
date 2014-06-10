package com.dominikgruber.fpinscala.chapter08

import com.dominikgruber.fpinscala.chapter06.{Chapter06, RNG, State}
import scala.annotation.tailrec

case class Gen[A](sample: State[RNG,A]) {

  /**
   * Exercise 06
   * Implement flatMap and then use it to implement sameParity and a more
   * dynamic version of listOfN. Put flatMap and listOfN in the Gen class, and
   * sameParity in the Gen companion object.
   */
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))
}

object Gen {

  /**
   * Exercise 04
   * Implement Gen.choose using this representation of Gen. Feel free to use
   * functions you've already written.
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val run: RNG => (Int, RNG) = (rng: RNG) => {
      def genInt: RNG => (Int, RNG) = (rng: RNG) => {
        val (i, rnd) = rng.nextInt
        if (i >= start && i < stopExclusive) (i, rnd)
        else genInt(rnd)
      }
      genInt(rng)
    }
    Gen(State(run))
  }

  // Much nicer reference implementation
  def choose2(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(Chapter06.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  /**
   * Exercise 05
   * Let's see what else we can implement using this representation of Gen. Try
   * implementing unit, boolean, and listOfN.
   */
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(Chapter06.nonNegativeInt).map(n => if (n % 2 == 0) true else false))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  /**
   * Exercise 06
   * Implement flatMap and then use it to implement sameParity and a more
   * dynamic version of listOfN. Put flatMap and listOfN in the Gen class, and
   * sameParity in the Gen companion object.
   */
  def sameParity(from: Int, to: Int): Gen[(Int,Int)] = {
    choose2(from, to).flatMap { i =>
      choose2(from, to).flatMap { j =>
        if (i % 2 == j % 2) Gen(State.unit(i, j))
        else Gen(State.unit(i, j + 1))
      }
    }
  }

  /**
   * Exercise 07
   * Implement union, for combining two generators of the same type into one, by
   * pulling values from each generator with equal likelihood.
   */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  /**
   * Exercise 08
   * Implement weighted, a version of union which accepts a weight for each Gen
   * and generates values from each Gen with probability proportional to its
   * weight.
   */
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    Gen(State(Chapter06.double).flatMap { d =>
      if (d < (g1._2 / (g1._2 + g2._2))) g1._1.sample
      else g2._1.sample
    })
  }
}