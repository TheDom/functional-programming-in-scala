package com.dominikgruber.fpinscala.chapter08

import com.dominikgruber.fpinscala.chapter05.Stream
import com.dominikgruber.fpinscala.chapter06.RNG
import Prop._

case class Prop(run: (TestCases, RNG) => Result) {

  /**
   * Exercise 09
   * Now that we have a representation of Prop, implement &&, and || for
   * manipulating Prop values. While we can implement &&, notice that in the
   * case of failure we aren't informed which property was responsible, the left
   * or the right.
   * (Optional): Can you devise a way of handling this, perhaps by allowing Prop
   * values to be assigned a tag or label which gets displayed in the event of a
   * failure?
   */
  def &&(p: Prop): Prop = Prop {
    (testCases: TestCases, rng: RNG) => {
      (this.run(testCases, rng), p.run(testCases, rng)) match {
        case (None, None) => None
        case (Some((failedCase1, success1)), Some((failedCase2, success2))) => Some((failedCase1 + " / " + failedCase2, success1 + success2))
        case (Some((failedCase1, success1)), _) => Some((failedCase1, success1))
        case (_, Some((failedCase2, success2))) => Some((failedCase2, success2))
      }
    }
  }

  def ||(p: Prop): Prop = Prop {
    (testCases: TestCases, rng: RNG) => {
      (this.run(testCases, rng), p.run(testCases, rng)) match {
        case (None, _) => None
        case (_, None) => None
        case (Some((failedCase1, success1)), Some((failedCase2, success2))) => Some((failedCase1 + " / " + failedCase2, success1 + success2))
      }
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) None else Some((a.toString, i))
      } catch { case e: Exception => Some((buildMsg(a, e), i)) }
    }.find(_.isDefined).getOrElse(None)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

