package com.dominikgruber.fpinscala.chapter06

import org.scalatest._

class Exercise09Spec extends FlatSpec with Matchers {
  import Chapter06._

  "mapViaFlatMap" should "work with double2" in {
    def double2_2: Rand[Double] =
      mapViaFlatMap(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

    double2_2(Simple(-10000000))._1 should be (0.3733188882470131)
  }

  "map2ViaFlatMap" should "work with randIntDouble" in {
    def both_2[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
      map2ViaFlatMap(ra, rb)((_, _))

    val randIntDouble_2: Rand[(Int, Double)] =
      both_2(int, double)

    randIntDouble_2(Simple(42))._1 should be ((16159453, 0.5967354853637516))
  }
}
