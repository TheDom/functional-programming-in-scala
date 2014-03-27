package com.dominikgruber.fpinscala.chapter05

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
   * Exercise 01
   * Write a function to convert a Stream to a List, which will force its
   * evaluation and let us look at it in the REPL. You can convert to the
   * regular List type in the standard library. You can place this and other
   * functions that operate on a Stream inside the Stream trait.
   */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /**
   * Exercise 02
   * Write the function take(n) for returning the first n elements of a Stream,
   * and drop(n) for skipping the first n elements of a Stream.
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
   * Exercise 03
   * Write the function takeWhile for returning all starting elements of a
   * Stream that match the given predicate.
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) =>
      val hh = h()
      if (p(hh)) Stream.cons(hh, t().takeWhile(p))
      else Empty
    case _ => Empty
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}