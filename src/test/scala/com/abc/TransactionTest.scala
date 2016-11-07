package com.abc

import org.joda.time.{DateTime, Days}
import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = Transaction(5, DateTime.now)
    t.isInstanceOf[Transaction] should be(true)
  }

  it should "test to see transactions record dates" in {
    val dateTime = DateTime.now
    val test = dateTime
    val t = Transaction(400, test)
    (Days.daysBetween(DateTime.now, test).getDays == 0) should be (true)
  }
}