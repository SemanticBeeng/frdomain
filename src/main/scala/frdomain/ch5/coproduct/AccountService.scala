package frdomain.ch5
package coproduct

import java.util.Date
import scalaz._
import Scalaz._
import Free._

import common._

import scala.language.higherKinds

import Injective._

sealed trait AccountAction[A]
  
case class Open(no: String, name: String, openingDate: Option[Date]) extends AccountAction[Option[Account]]
case class Close(a: Account, closeDate: Option[Date]) extends AccountAction[Account]
case class Debit(a: Account, amount: Amount) extends AccountAction[Account]
case class Credit(a: Account, amount: Amount) extends AccountAction[Account]
case class GetBalance(a: Account) extends AccountAction[Balance]

class AccountService[F[_]](implicit I: Inject[AccountAction, F]) { 
  def open(no: String, name: String, openingDate: Option[Date]): Free.FreeC[F, Option[Account]] = 
    lift(Open(no, name, openingDate))
  
  def close(a: Account, closeDate: Option[Date]): Free.FreeC[F, Account] =
    lift(Close(a, closeDate))
  
  def debit(a: Account, amount: Amount): Free.FreeC[F, Account] =
    lift(Debit(a, amount))
  
  def credit(a: Account, amount: Amount): Free.FreeC[F, Account] =
    lift(Credit(a, amount))
  
  def balance(a: Account): Free.FreeC[F, Balance] =
    lift(GetBalance(a))
}

object AccountService {
  implicit def instance[F[_]](implicit I: Inject[AccountAction, F]): AccountService[F] = new AccountService[F]
}

object AccountServiceInterpreter extends (AccountAction ~> Id) {
  def apply[A](r: AccountAction[A]) = r match {
    case Open(no, name, odate) => Account(no, name, odate.map(identity).getOrElse(today)).some

    case Close(a, cdate) => 
      val cd = cdate.map(identity).orElse(Some(today))
      a.copy(dateOfClosing = cd)

    case Debit(a, amount) => {
      if (a.balance.amount < amount) throw new Exception("Insufficient balance")
      else a.copy(balance = Balance(a.balance.amount - amount))
    }

    case Credit(a, amount) => a.copy(balance = Balance(a.balance.amount + amount))
    case GetBalance(a: Account) => a.balance
  }
}
