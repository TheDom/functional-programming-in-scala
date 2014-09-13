package com.dominikgruber.fpinscala.chapter12

import com.dominikgruber.fpinscala.chapter10.Foldable
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

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id,A,B](fa)(f)
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