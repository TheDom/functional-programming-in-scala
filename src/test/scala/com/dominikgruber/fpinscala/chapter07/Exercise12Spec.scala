package com.dominikgruber.fpinscala.chapter07

import org.scalatest._
import java.util.concurrent.Executors

class Exercise12Spec extends FlatSpec with Matchers {
  import Par._

  "choiceMap" should "pick the right value" in {
    val pool = Executors.newFixedThreadPool(1)
    choiceMap(unit(2))(Map(1 -> unit("1"), 2 -> unit("2")))(pool).get should be ("2")
    pool.shutdown()
  }
}
