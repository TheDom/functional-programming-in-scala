package com.dominikgruber.fpinscala.chapter08

import com.dominikgruber.fpinscala.chapter05.Stream
import com.dominikgruber.fpinscala.chapter06.{RNG, Simple}
import Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

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
    (max, testCases, rng) => {
      (this.run(max, testCases, rng), p.run(max, testCases, rng)) match {
        case (None, None) => None
        case (Some((failedCase1, success1)), Some((failedCase2, success2))) => Some((failedCase1 + " / " + failedCase2, success1 + success2))
        case (Some((failedCase1, success1)), _) => Some((failedCase1, success1))
        case (_, Some((failedCase2, success2))) => Some((failedCase2, success2))
      }
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, testCases, rng) => {
      (this.run(max, testCases, rng), p.run(max, testCases, rng)) match {
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
  type MaxSize = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
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

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  val smallInt = Gen.choose(-10,10)

  val maxProp = forAll(Gen.listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Some((msg, n)) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case None =>
        println(s"+ OK, passed $testCases tests.")
    }

  /**
   * Exercise 15
   * Write a property to verify the behavior of List.sorted, which you can use
   * to sort (among other things) a List[Int]. For instance, List(2,1,3).sorted
   * is equal to List(1,2,3).
   */
  def sortedListProp = forAll(Gen.listOf1(smallInt)) { l =>
    l.sorted == l.sorted.reverse.reverse // Simplistic Check
  }
}

