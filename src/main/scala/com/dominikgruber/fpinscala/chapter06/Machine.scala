package com.dominikgruber.fpinscala.chapter06

/**
 * Exercise 11 (hard, optional)
 * To gain experience with the use of State, implement a finite state automaton
 * that models a simple candy dispenser. The machine has two types of input:
 * You can insert a coin, or you can turn the knob to dispense candy. It can be
 * in one of two states: locked or unlocked. It also tracks how many candies are
 * left and how many coins it contains.
 *
 * The rules of the machine are as follows:
 * - Inserting a coin into a locked machine will cause it to unlock if there is
 * any candy left.
 * - Turning the knob on an unlocked machine will cause it to dispense candy and
 * become locked.
 * - Turning the knob on a locked machine or inserting a coin into an unlocked
 * machine does nothing.
 * - A machine that is out of candy ignores all inputs.
 *
 * The method simulateMachine should operate the machine based on the list of
 * inputs and return the number of coins and candies left in the machine at the
 * end. For example, if the input Machine has 10 coins and 5 candies, and a
 * total of 4 candies are successfully bought, the output should be (14, 1).
 */

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(i => modify((m: Machine) => (i, m) match {
        case (Coin, Machine(true, candy, coin)) if candy > 0 =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) if candy > 0 =>
          Machine(true, candy - 1, coin)
        case _ => m
      })))
      m <- get
    } yield (m.coins, m.candies)
}