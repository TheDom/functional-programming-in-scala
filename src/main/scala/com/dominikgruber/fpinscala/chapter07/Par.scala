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

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  /**
   * Exercise 05 (hard)
   * Let's write this function, called sequence. No additional primitives are
   * required.
   */
  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List[A]()))((h, t) => map2(h, t)(_ :: _))

  /**
   * Exercise 06
   * Implement parFilter, which filters elements of a list in parallel.
   */
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = l.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  /**
   * Exercise 11
   * Implement choiceN and then choice in terms of choiceN.
   */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val idx = run(es)(n).get
      choices(idx)(es)
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(if (_) 0 else 1))(List(t, f))
}
