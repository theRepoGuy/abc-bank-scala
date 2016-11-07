package com.abc

import org.joda.time.{DateTime, Days}

import scala.collection.mutable.ListBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int, val transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else{
      val now = DateTime.now
      transactions += Transaction(amount, now)
    }
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else{
      val now = DateTime.now
      transactions += Transaction(-amount, now)
    }
  }

  private def computeDailyInterest(principle: Double, rate: Double): Double = {
    (accountAge * principle * rate) / 365
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) computeDailyInterest(amount, 0.001)
        else computeDailyInterest(1 + (amount - 1000), 0.002)
      case Account.MAXI_SAVINGS =>
        computeDailyInterest(amount, (if (checkLastTenWithdraws) 0.05 else 0.001))
      case _ =>
        computeDailyInterest(amount, 0.001)
    }
  }

  def checkLastTenWithdraws : Boolean = {

    val now = DateTime.now
    transactions.foldLeft(true){ (state, transaction) =>
        if (Days.daysBetween(now, transaction.date).getDays < 10 && transaction.amount > 0.0)
          state && true
        else
          state && false
      }

  }

  def accountAge: Int = {
    val now = DateTime.now
    val diff = Days.daysBetween(now, transactions.head.date).getDays
    if(diff > 0) diff
    else 1
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

}