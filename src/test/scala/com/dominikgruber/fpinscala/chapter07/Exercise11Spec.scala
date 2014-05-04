package com.dominikgruber.fpinscala.chapter07

import org.scalatest._
import java.util.concurrent.Executors

class Exercise11Spec extends FlatSpec with Matchers with BeforeAndAfter {
  import Par._

  var pool = Executors.newFixedThreadPool(1)

  after {
    pool.shutdown()
  }

  "choiceN" should "return the correct result" in {
    choiceN(unit(1))(List(unit(1), unit(2), unit(3)))(pool).get should be (2)
  }

  "choice" should "pick true" in {
    choice(unit(true))(unit(1), unit(2))(pool).get should be (1)
  }

  it should "pick false" in {
    choice(unit(false))(unit(1), unit(2))(pool).get should be (2)
  }
}
