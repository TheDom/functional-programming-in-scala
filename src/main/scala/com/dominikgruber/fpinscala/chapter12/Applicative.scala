package com.dominikgruber.fpinscala.chapter12

import com.dominikgruber.fpinscala.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  /*
   * See exercise 2 for the implementation
   * def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
   */
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  /**
   * Exercise 01
   * Transplant the implementations of as many combinators as you can from Monad
   * to Applicative, using only map2 and unit, or methods implemented in terms
   * of them.
   */
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(x => x)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  /**
   * Exercise 02 (hard)
   * The name applicative comes from the fact that we can formulate the
   * Applicative interface using an alternate set of primitives, unit and the
   * function apply, rather than unit and map2. Show that this formulation is
   * equivalent in expressiveness by defining map2 and map in terms of unit and
   * apply. Also establish that apply can be implemented in terms of map2 and
   * unit.
   */
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def mapViaApplyAndUnit[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  /**
   * Exercise 03
   * The apply method is useful for implementing map3, map4, and so on, and the
   * pattern is straightforward. Implement map3 and map4 using only unit, apply,
   * and the curried method available on functions.
   */
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)
}