package com.dominikgruber.fpinscala.chapter07

import org.scalatest._
import java.util.concurrent.{Executors, ExecutorService}

class Exercise04Spec extends FlatSpec with Matchers {

  "asyncF" should "evaluates the result of the function asynchronously" in {
    val pool: ExecutorService = Executors.newFixedThreadPool(1)
    Par.asyncF((i: Int) => i + 1)(1)(pool).get should be (2)
    pool.shutdown()
  }
}
