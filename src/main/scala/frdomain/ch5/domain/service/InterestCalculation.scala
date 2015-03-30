package frdomain.ch5
package domain
package service

import scalaz._
import Scalaz._

trait InterestCalculation[Account, Amount] {
  type Valid[A] = NonEmptyList[String] \/ A

  def computeInterest: Kleisli[Valid, Account, Amount]
}
