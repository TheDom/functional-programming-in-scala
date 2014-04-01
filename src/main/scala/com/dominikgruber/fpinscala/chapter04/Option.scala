package com.dominikgruber.fpinscala.chapter04

sealed trait Option[+A] {

  /**
   * Exercise 1
   * Implement all of the preceding functions on Option. As you implement each
   * function, try to think about what it means and in what situations you’d
   * use it.
   */
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    if (map(f) getOrElse false) this
    else None
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Chapter04 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
   * Exercise 2
   * Implement the variance function in terms of flatMap. If the mean of a
   * sequence is m, the variance is the mean of math.pow(x - m, 2) for each
   * element x in the sequence.
   */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }

  /**
   * Exercise 3
   * Write a generic function map2 that combines two Option values using a
   * binary function. If either Option value is None, then the return value is
   * too.
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }

  // Official Solution
  def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /**
   * Exercise 4
   * Write a function sequence that combines a list of Options into one option
   * containing a list of all the Some values in the original list. If the
   * original list contains None even once, the result of the function should be
   * None; otherwise the result should be Some with a list of all the values.
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go(a: List[Option[A]], acc: List[A]): Option[List[A]] = a match {
      case Nil => Some(acc)
      case Some(x) :: xs => go(xs, acc :+ x)
      case None :: xs => None
    }
    go(a, List[A]())
  }

  // Official Solution
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence_1(t) map (hh :: _))
    }

  /**
   * Exercise 5
   * Implement this function. It’s straightforward to do using map and sequence,
   * but try for a more efficient implementation that only looks at the list
   * once. In fact, implement sequence in terms of traverse.
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => f(x) flatMap(xx => traverse(xs)(f) map (xx :: _))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}