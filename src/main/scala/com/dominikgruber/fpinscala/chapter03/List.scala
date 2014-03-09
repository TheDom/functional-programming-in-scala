package com.dominikgruber.fpinscala.chapter03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
   * Exercise 02
   * Implement the function tail for “removing” the first element of a List.
   * Note that the function takes constant time. What are different choices
   * you could make in your implementation if the List is Nil?
   * We’ll return to this question in the next chapter.
   */
  def tail[A](l: List[A]) = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  /**
   * Exercise 03
   * Using the same idea, implement the function setHead for replacing the first
   * element of a List with a different value.
   */
  def setHead[A](head: A, l: List[A]) = l match {
    case Nil => Cons(head, Nil)
    case Cons(_, t) => Cons(head, t)
  }

  /**
   * Exercise 04
   * Generalize tail to the function drop, which removes the first n elements
   * from a list. Note that this function takes time proportional only to the
   * number of elements being dropped—we don’t need to make a copy of the
   * entire List.
   */
  def drop[A](n: Int, l: List[A]): List[A] =
    if (n <= 0) l
    else drop(n - 1, tail(l))

  /**
   * Exercise 05
   * Implement dropWhile, which removes elements from the List prefix as long as
   * they match a predicate.
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  /**
   * Exercise 06
   * Not everything works out so nicely. Implement a function, init, which
   * returns a List consisting of all but the last element of a List. So, given
   * List(1,2,3,4), init will return List(1,2,3). Why can’t this function be
   * implemented in constant time like tail?
   */
  def init[A](l: List[A]): List[A] = {
    def acc(l1: List[A], l2: List[A]): List[A] = l1 match {
      case Nil => l2
      case Cons(h, Nil) => l2
      case Cons(h, t) => acc(t, append(l2, List(h)))
    }
    acc(l, Nil)
  }
}