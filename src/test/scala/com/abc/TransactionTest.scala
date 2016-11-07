package com.abc

import org.joda.time.{DateTime, Days}
import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = Transaction(5, DateTime.now)
    t.isInstanceOf[Transaction] should be(true)
  }

  it should "test to see transactions record dates" in {
    val t1 = Transaction(500, DateTime.now.minusDays(10))
    val t2 = Transaction(400, DateTime.now.minusDays(10))
    (Days.daysBetween(DateTime.now, t1.date).getDays > 0) should be (true)
  }
}