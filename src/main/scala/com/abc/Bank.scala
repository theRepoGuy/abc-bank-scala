package com.abc

import scala.collection.mutable.ListBuffer

class Bank {

  val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    customers.foldLeft("Customer Summary"){ (summary, customer) =>
        summary + "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")"
    }
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    customers.foldLeft(0.0)((total, c) => total + c.totalInterestEarned)
  }

  def getFirstCustomer: String = {
      if (!customers.isEmpty)
        customers.head.name
      else
        "No Customers"
  }

}


