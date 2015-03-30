package frdomain.ch5
package domain
package app

import scalaz._
import Scalaz._
import Kleisli._

import service.interpreter.{ AccountService, InterestCalculation }
import repository.interpreter.AccountRepositoryInMemory
import service.Checking

object App {

  import AccountService._
  import InterestCalculation._

  val postTransactions = 
    for {
      a <- open("a-123", "debasish ghosh", None, None, Checking)
      _ <- credit(a.no, 10000)
      _ <- credit(a.no, 15000)
      b <- debit(a.no, 13000)
    } yield b

  val composite = postTransactions >=> computeInterest

  val x = composite(AccountRepositoryInMemory)
}
