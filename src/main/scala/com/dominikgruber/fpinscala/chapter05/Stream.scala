package com.dominikgruber.fpinscala.chapter05

sealed trait Stream[+A] {
  import Stream._

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
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  /**
   * Exercise 02
   * Write the function take(n) for returning the first n elements of a Stream,
   * and drop(n) for skipping the first n elements of a Stream.
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n == 1 => t()
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
   * Exercise 03
   * Write the function takeWhile for returning all starting elements of a
   * Stream that match the given predicate.
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      val hh = h()
      if (p(hh)) Stream.cons(hh, t().takeWhile(p))
      else Empty
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
   * Exercise 4
   * Implement forAll, which checks that all elements in the Stream match a
   * given predicate. Your implementation should terminate the traversal as
   * soon as it encounters a non-matching value.
   */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /**
   * Exercise 5
   * Use foldRight to implement takeWhile.
   */
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) =>
      if (p(h)) cons(h, t)
      else Empty
    )

  /**
   * Exercise 6
   * Implement headOption using foldRight. (hard)
   */
  def headOption2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  /**
   * Exercise 7
   * Implement map, filter, append, and flatMap using foldRight. The append
   * method should be non-strict in its argument.
   */
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) =>
      if (p(h)) cons(h, t)
      else t
    )

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((h, t) => f(h).append(t))
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

  /**
   * Exercise 8
   * Generalize ones slightly to the function constant which returns an infinite
   * Stream of a given value.
   */
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  /**
   * Exercise 9
   * Write a function that generates an infinite stream of integers, starting
   * from n, then n + 1, n + 2, etc.
   */
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  /**
   * Exercise 10
   * Write a function fibs that generates the infinite stream of Fibonacci
   * numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
   */
  def fibs: Stream[Int] = {
    def innerFibs(a: Int, b: Int): Stream[Int] = {
      cons(a, innerFibs(b, a + b))
    }
    innerFibs(0, 1)
  }

  /**
   * Exercise 11
   * We can write a more general stream building function. It takes an initial
   * state, and a function for producing both the next state and the next value
   * in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }
}