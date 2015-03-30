package frdomain.ch5
package domain
package service

import scalaz._
import Scalaz._
import Kleisli._

import repository.AccountRepository


trait ReportingService[Amount] {
  type Valid[A] = NonEmptyList[String] \/ A
  type ReportOperation[A] = Kleisli[Valid, AccountRepository, A]

  def balanceByAccount: ReportOperation[Seq[(String, Amount)]]
} 
