package com.dominikgruber.fpinscala.chapter12

import com.dominikgruber.fpinscala.chapter06.State
import com.dominikgruber.fpinscala.chapter10.{Monoid, Foldable}
import com.dominikgruber.fpinscala.chapter11.Functor

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_],A,B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_],A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  /**
   * Exercise 14 (hard)
   * Implement map in terms of traverse as a method on Traverse[F]. This
   * establishes that Traverse is an extension of Functor and that the traverse
   * function is a generalization of map (for this reason we sometimes call these
   * traversable functors). Note that in implementing map, you can call traverse
   * with your choice of Applicative[G].
   */
  type Id[A] = A

  implicit val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id,A,B](fa)(f)


  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero
      override def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = M.op(m1, m2)
    }

  override def foldMap[A,M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M,x]})#f,A,Nothing](as)(f)(monoidApplicative(mb))


  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = traverseS(fa)((a: A) => (for {
    s1 <- State.get[S]
    (b, s2) = f(a, s1)
    _  <- State.set(s2)
  } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  /**
   * Exercise 16
   * There’s an interesting consequence of being able to turn any traversable
   * functor into a reversed list—we can write, once and for all, a function to
   * reverse any traversable functor! Write this function, and think about what
   * it means for List, Tree, and other traversable functors.
   *
   * It should obey the following law, for all x and y of the appropriate types:
   *  toList(reverse(x)) ++ toList(reverse(y)) ==
   *  reverse(toList(y) ++ toList(x))
   */
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  /**
   * Exercise 17
   * Use mapAccum to give a default implementation of foldLeft for the Traverse
   * trait.
   */
  override def foldLeft[A, B](l: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(l, z)((a, b) => ((), f(b, a)))._2

  /**
   * Exercise 18
   * Use applicative functor products to write the fusion of two traversals.
   * This function will, given two functions f and g, traverse fa a single time,
   * collecting the results of both functions at once.
   */
  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f,A,B](fa)(a => (f(a), g(a)))(G product H)

  /**
   * Exercise 19
   * Implement the composition of two Traverse instances.
   */
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)(ga => G.traverse(ga)(f))
    }
  }
}

object Traverse {

  /**
   * Exercise 13
   * Write Traverse instances for List, Option, and Tree.
   */
  case class Tree[+A](head: A, tail: List[Tree[A]])

  def listTraverse = new Traverse[List] {
    override def traverse[G[_],A,B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  def optionTraverse = new Traverse[Option] {
    override def traverse[G[_],A,B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = {
      fa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None => G.unit(None)
      }
    }
  }

  def treeTraverse = new Traverse[Tree] {
    override def traverse[G[_],A,B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)(a => traverse(a)(f)))(Tree(_, _))
    }
  }
}