package com.dominikgruber.fpinscala.chapter10

import com.dominikgruber.fpinscala.chapter03.{Branch, Leaf, Tree}

trait Foldable[F[_]] {
  import Chapter10._

  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  /**
   * Exercise 15
   * Any Foldable structure can be turned into a List. Write this conversion in
   * a generic way:
   */
  def toList[A](fa: F[A]): List[A] = {
    foldRight(fa)(List[A]())(_ :: _)
  }
}

/**
 * Exercise 12
 * Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
 * Remember that foldRight, foldLeft, and foldMap can all be implemented in
 * terms of each other, but that might not be the most efficient
 * implementation.
 */
object ListFoldable extends Foldable[List] {

  override def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B): B =
    as.foldLeft(z)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Chapter10._

  override def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A,B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A,B](as: IndexedSeq[A])(z: B)(f: (B,A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A,B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {

  override def foldRight[A,B](as: Stream[A])(z: B)(f: (A,B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A,B](as: Stream[A])(z: B)(f: (B,A) => B): B =
    as.foldLeft(z)(f)
}

/**
 * Exercise 13
 * Recall the binary Tree data type from chapter 3. Implement a Foldable
 * instance for it.
 */
object TreeFoldable extends Foldable[Tree] {

  override def foldRight[A,B](as: Tree[A])(z: B)(f: (A,B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }

  override def foldLeft[A,B](as: Tree[A])(z: B)(f: (B,A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldMap[A,B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
}

/**
 * Exercise 14
 * Write a Foldable[Option] instance.
 */

object ObjectFoldable extends Foldable[Option] {

  override def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B = as match {
    case None => z
    case Some(a) => f(a, z)
  }

  override def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  override def foldMap[A,B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }
}