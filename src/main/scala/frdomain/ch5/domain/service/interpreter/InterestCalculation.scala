package frdomain.ch5
package domain
package service
package interpreter

import scalaz._
import Scalaz._
import Kleisli._

import model.{ Account, Balance }
import model.common._

class InterestCalculationInterpreter extends InterestCalculation[Account, Amount] {
  def computeInterest = kleisli[Valid, Account, Amount] { (account: Account) =>
    if (account.dateOfClose isDefined) NonEmptyList(s"Account ${account.no} is closed").left
    else (account.balance.amount * 1.1).right
  }
}

object InterestCalculation extends InterestCalculationInterpreter

