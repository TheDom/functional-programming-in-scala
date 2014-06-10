package com.dominikgruber.fpinscala.chapter08

trait Prop_Ex03 {
  def check: Boolean

  /**
   * Exercise 03
   * Assuming the following definition of Prop, implement && as a method of
   * Prop:
   *
   * trait Prop { def check: Boolean }
   */
  def &&(p: Prop_Ex03): Prop_Ex03 = new Prop_Ex03 {
    def check = Prop_Ex03.this.check && p.check
  }
}