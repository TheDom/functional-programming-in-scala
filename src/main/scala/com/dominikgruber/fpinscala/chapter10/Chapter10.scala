package com.dominikgruber.fpinscala.chapter10

import com.dominikgruber.fpinscala.chapter07.Par
import com.dominikgruber.fpinscala.chapter07.Par.Par
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

  /**
   * Exercise 05
   * Write this function.
   */
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(a => f(a)).foldRight(m.zero)(m.op)

  // Better reference implementation
  // as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  /**
   * Exercise 06 (hard)
   * The foldMap function can be implemented using either foldLeft or foldRight.
   * But you can also write foldLeft and foldRight using foldMap! Try it.
   */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  /**
   * Exercise 07
   * Implement a foldMap for IndexedSeq. Your implementation should use the
   * strategy of splitting the sequence in two, recursively processing each
   * half, and then adding the answers together with the monoid.
   */
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length == 0) m.zero
    else if (v.length == 1) f(v(0))
    else {
      val s = v.splitAt(v.length / 2)
      m.op(foldMapV(s._1, m)(f), foldMapV(s._2, m)(f))
    }

  def foldMapV_org[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length >= 4) {
      val s = v.splitAt(v.length / 2)
      m.op(foldMapV(s._1, m)(f), foldMapV(s._2, m)(f))
    } else {
      v.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
    }

  /**
   * Exercise 08 (hard)
   * Also implement a parallel version of foldMap using the library we developed
   * in chapter 7. Hint: Implement par, a combinator to promote Monoid[A] to a
   * Monoid[Par[A]], and then use this to implement parFoldMap.
   */
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))((a: A) => Par.unit(f(a)))

  // Reference implementation
  // Par.parMap(v)(f).flatMap { bs =>
  //  foldMapV(bs, par(m))(b => Par.async(b))
  // }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a: Par[A], b: Par[A]) = Par.map2(a, b)(m.op)
    def zero = Par.unit(m.zero)
  }

  /**
   * Exercise 09 (hard)
   * Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll
   * need to come up with a creative Monoid.
   */
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Reference implementation
    // (Int, Int, Boolean) = (Min, Max, IsOrdered)
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }
    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }
}
