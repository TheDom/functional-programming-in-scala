package com.dominikgruber.fpinscala.chapter11

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}