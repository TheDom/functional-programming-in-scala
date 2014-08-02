package com.dominikgruber.fpinscala.chapter11

// I'm not using my own list anymore because it does not implement fill()
// import com.dominikgruber.fpinscala.chapter03.{Cons, Nil, List}
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

  /**
   * Exercise 03
   * The sequence and traverse combinators should be pretty familiar to you by
   * now, and your implementations of them from various prior chapters are
   * probably all very similar. Implement them once and for all on Monad[F].
   */
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(x => x)

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  /**
   * Exercise 04
   * Implement replicateM.
   */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  /**
   * Exercise 06 (hard)
   * Here’s an example of a function you haven’t seen before. Implement the
   * function filterM. It’s a bit like filter, except that instead of a function
   * from A => Boolean, you have an A => F[Boolean]. (Replacing various ordinary
   * functions like this with the monadic equivalent often yields interesting
   * results.) Implement this function, and then think about what it means for
   * various data types you’ve implemented.
   */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil => unit(Nil)
    case x :: xs =>
      flatMap(f(x))(b =>
        if (b) map(filterM(xs)(f))(x :: _)
        else filterM(xs)(f)
      )
  }

  /**
   * Exercise 07
   * Implement this function.
   */
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  /**
   * Exercise 08 (hard)
   * Implement flatMap in terms of compose. It seems that we’ve found another
   * minimal set of monad combinators: compose and unit.
   */
  def flatMapViaCompose[A,B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  /**
   * Exercise 12
   * There’s a third minimal set of monadic combinators: map, unit, and join.
   * Implement join.
   */
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  /**
   * Exercise 13
   * Implement either flatMap or compose in terms of join.
   */
  def flatMapViaJoin[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def composeViaJoin[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))
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
      ma flatMap f
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

  // Reference implementation
  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
    def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
      st flatMap f
  }
}