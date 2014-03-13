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