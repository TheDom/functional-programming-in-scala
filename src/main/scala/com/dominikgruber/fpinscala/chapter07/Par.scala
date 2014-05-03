package com.dominikgruber.fpinscala.chapter07

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  /**
   * Exercise 03 (optional, hard)
   * Fix the implementation of map2 so that it respects the contract of timeouts
   * on Future.
   */
  def map2_2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???

  /**
   * Exercise 04
   * This API already enables a rich set of operations. Here's a simple example:
   * using lazyUnit, write a function to convert any function A => B to one that
   * evaluates its result asynchronously:
   */
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
}
