package com.dominikgruber.fpinscala.chapter03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
   * Exercise 25
   * Write a function size that counts the number of nodes (leaves and branches)
   * in a tree.
   */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /**
   * Exercise 26
   * Write a function maximum that returns the maximum element in a Tree[Int].
   * (Note: in Scala, you can use x.max(y) or x max y to compute the maximum of
   * two integers x and y.)
   */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  /**
   * Exercise 27
   * Write a function depth that returns the maximum path length from the root
   * of a tree to any leaf.
   */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  /**
   * Exercise 28
   * Write a function map, analogous to the method of the same name on List,
   * that modifies each element in a tree with a given function.
   */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}