package com.dominikgruber.fpinscala.chapter07

import org.scalatest._
import java.util.concurrent.Executors

class Exercise13Spec extends FlatSpec with Matchers with BeforeAndAfter {
  import Par._

  var pool = Executors.newFixedThreadPool(1)

  after {
    pool.shutdown()
  }

  "chooser" should "return the correct result" in {
    chooser(unit(1))((a: Int) => unit(a + 1))(pool).get should be (2)
  }

  "choiceN" should "return the correct result" in {
    choiceN_2(unit(1))(List(unit(1), unit(2), unit(3)))(pool).get should be (2)
  }

  "choice" should "pick true" in {
    choice_2(unit(true))(unit(1), unit(2))(pool).get should be (1)
  }

  it should "pick false" in {
    choice_2(unit(false))(unit(1), unit(2))(pool).get should be (2)
  }
}
