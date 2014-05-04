package com.dominikgruber.fpinscala.chapter07

import org.scalatest._
import java.util.concurrent.Executors

class Exercise14Spec extends FlatSpec with Matchers with BeforeAndAfter {
  import Par._

  var pool = Executors.newFixedThreadPool(1)

  after {
    pool.shutdown()
  }

  "join" should "turn Par[Par[A]] to Par[A]" in {
    join(unit(unit(2)))(pool).get should be (2)
  }

  "flatMap" should "return the correct result" in {
    flatMap(unit(1))((a: Int) => unit(a + 1))(pool).get should be (2)
  }

  "joinViaFlatMap" should "turn Par[Par[A]] to Par[A]" in {
    joinViaFlatMap(unit(unit(2)))(pool).get should be (2)
  }
}
