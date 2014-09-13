package com.dominikgruber.fpinscala.chapter12

import com.dominikgruber.fpinscala.chapter10.Monoid
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

  /**
   * Exercise 08
   * Just like we can take the product of two monoids A and B to give the
   * monoid (A, B), we can take the product of two applicative functors.
   * Implement this function:
   */
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  /**
   * Exercise 09 (hard)
   * Applicative functors also compose another way! If F[_] and G[_] are
   * applicative functors, then so is F[G[_]]. Implement this function:
   */
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))
      override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C) =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

  /**
   * Exercise 12
   * On the Applicative trait, implement sequence over a Map rather than a List:
   */
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map[K, V]())) { case (fm, (k, fv)) =>
      map2(fm, fv)((m, v) => m.updated(k, v))
    }
}

object Applicative {
  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M, x] })#f] {
    def unit[A](a: => A): M = M.zero
    override def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = M.op(m1,m2)
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    def zero: Option[A] = None
  }

  val optionApplicative = monoidApplicative(optionMonoid[Int])
}