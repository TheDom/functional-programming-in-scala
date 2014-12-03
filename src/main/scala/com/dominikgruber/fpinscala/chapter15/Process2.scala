package com.dominikgruber.fpinscala.chapter15

import com.dominikgruber.fpinscala.chapter12.Monad

trait Process2[F[_],O] {
  import Process2._

  def onHalt(f: Throwable => Process2[F,O]): Process2[F,O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => Process2[F,O]): Process2[F,O] =
    this.onHalt {
      case End => p
      case err => Halt(err)
    }

  def flatMap[O2](f: O => Process2[F,O2]): Process2[F,O2] = this match {
    case Halt(err) => Halt(err)
    case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
    case Await(req,recv) => Await(req, recv andThen (_ flatMap f))
  }

  def onComplete(p: => Process2[F,O]): Process2[F,O] = this.onHalt {
    case End => p.asFinalizer
    case err => p.asFinalizer ++ Halt(err)
  }

  def asFinalizer: Process2[F,O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e) => Halt(e)
    case Await(req,recv) => await(req) {
      case Left(Kill) => this.asFinalizer
      case x => recv(x)
    }
  }
}

object Process2 {
  case class Await[F[_],A,O](req: F[A], recv: Either[Throwable, A] => Process2[F,O]) extends Process2[F,O]
  case class Emit[F[_],O](head: O, tail: Process2[F,O]) extends Process2[F,O]
  case class Halt[F[_],O](err: Throwable) extends Process2[F,O]
  case object End extends Exception
  case object Kill extends Exception

  def Try[F[_],O](p: => Process2[F,O]): Process2[F,O] =
    try p
    catch { case e: Throwable => Halt(e) }

  def await[F[_],A,O](req: F[A])(recv: Either[Throwable,A] => Process2[F,O]): Process2[F,O] =
    Await(req, recv)
}

trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](a: F[A]): F[Either[Throwable,A]]
  def fail[A](t: Throwable): F[A]
}