package com.dominikgruber.fpinscala.chapter11

import com.dominikgruber.fpinscala.chapter03.List
import com.dominikgruber.fpinscala.chapter04.{Some, Option}
import com.dominikgruber.fpinscala.chapter05.Stream
import com.dominikgruber.fpinscala.chapter06.State
import com.dominikgruber.fpinscala.chapter07.Par
import com.dominikgruber.fpinscala.chapter07.Par.Par
import com.dominikgruber.fpinscala.chapter08.Gen

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
}

object Monad {

  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  /**
   * Exercise 01
   * Write monad instances for Par, Parser, Option, Stream, and List.
   */
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  /*
  val parserMonad = new Monad[Parser] {
    def unit[A](a: => A): Parser[A] = Parser.unit(a)
    def flatMap[A,B](ma: Parser[A])(f: A => Parser[B]): Parser[B] =
      Parsers.flatMap(ma)(f)
  }
   */

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] =
      List.flatMap(ma)(f)
  }

  /**
   * Exercise 02
   * State looks like it would be a monad too, but it takes two type arguments
   * and you need a type constructor of one argument to implement Monad. Try to
   * implement a State monad, see what issues you run into, and think about
   * possible solutions. We’ll discuss the solution later in this chapter.
   */
  /*
  val stateMonad = new Monad[State] {
    def unit[S,A](a: => A): State[S,A] = ???
    def flatMap[S1,A,S2,B](ma: State[S1,A])(f: State[S1,A] => State[S2,B]): State[S2,B] = ???
  }
   */
}