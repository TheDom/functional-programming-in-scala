package com.dominikgruber.fpinscala.chapter11

/**
 * Exercise 17
 * Implement map and flatMap as methods on this class, and give an
 * implementation for Monad[Id].
 */
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] =
      ma.flatMap(f)
  }
}