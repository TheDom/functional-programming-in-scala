package com.dominikgruber.fpinscala.chapter09

import com.dominikgruber.fpinscala.chapter08._
import com.dominikgruber.fpinscala.chapter08.Prop._
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  /**
   * Exercise 04 (hard, optional)
   * Using map2 and succeed, implement the listOfN combinator from earlier:
   */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List[A]())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  /**
   * Exercise 03 (hard)
   * Now that we have map2, is many really primitive? Let's think about what
   * many(p) will do. It tries running p, followed by many(p) again, and again,
   * and so on until attempting to parse p fails. It will accumulate the results
   * of all successful runs of p into a list. As soon as p fails the parser
   * returns the empty List.
   * Before continuing, see if you can define many in terms of or, map2, and
   * succeed.
   */
  def many[A](p: Parser[A]): Parser[List[A]] =
    or(succeed(List[A]()), map2(p, many(p))(_ :: _))
    // map2(p, many(p))(_ :: _) or succeed(List())

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  /**
   * Exercise 07
   * Implement product and map2 in terms of flatMap.
   */
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    // flatMap(p)(a => flatMap(p2)(b => succeed((a, b))))
    flatMap(p)(a => map(p2)(b => (a, b)))

  def map2ViaFlatMap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    // flatMap(p)(a => flatMap(p2)(b => succeed(f(a, b))))
    flatMap(p)(a => map(p2)(b => f(a, b)))

  /**
   * Exercise 08
   * map is no longer primitive. Express it in terms of flatMap and/or other
   * combinators.
   */
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  /**
   * Exercise 01
   * Using product, implement the now-familiar combinator map2 and then use this
   * to implement many1 in terms of many. Note that we could have chosen to make
   * map2 primitive and defined product in terms of map2 as we've done in
   * previous chapters. The choice is up to you.
   */
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)
    // map(product(p, p2))((ab) => f(ab._1, ab._2))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  /**
   * Exercise 06
   * Using flatMap and any other combinators, write the context-sensitive parser
   * we could not express above.
   * Suppose we want to parse a single digit, like '4', followed by that many 'a'
   * characters (this sort of problem should feel familiar from previous
   * chapters). Examples of valid input are "0", "1a", "2aa", "4aaaa", and so
   * on. This is an example of a context-sensitive grammar.
   */
  var ex06 = "\\d+".r.flatMap(s => listOfN(s.toInt, char('a')))

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B>:A](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def product[B>:A](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)

    def map2[B,C](p2: => Parser[B])(f: (A,B) => C): Parser[C] = self.map2(p, p2)(f)
    def many1 = self.many1(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')
  }

  case class ParseError(stack: List[(Location,String)])

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    /**
     * Exercise 02 (hard)
     * Try coming up with laws to specify the behavior of product.
     */
    def productLaw[A,B](p: Parser[A], p2: Parser[B], p3: Parser[B])(in: Gen[String], in2: Gen[String]): Prop =
      forAll(in)(s => (p ** p2) ** p3 == p ** (p2 ** p3))

    /* def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      } */
  }
}