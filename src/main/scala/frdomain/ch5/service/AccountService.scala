package frdomain.ch5
package service

import java.util.Date
import scalaz._
import Scalaz._
import Free._

import common._


sealed trait AccountActionF[+A]
  
case class Open[+A](no: String, name: String, openingDate: Option[Date], onResult: Account => A) extends AccountActionF[A]
case class Close[+A](no: String, closeDate: Option[Date], onResult: Account => A) extends AccountActionF[A]
case class Debit[+A](no: String, amount: Amount, onResult: Account => A) extends AccountActionF[A]
case class Credit[+A](no: String, amount: Amount, onResult: Account => A) extends AccountActionF[A]
case class GetBalance[+A](no: String, onResult: Balance => A) extends AccountActionF[A]
case class Fails[+A](e: Throwable) extends AccountActionF[A]

object AccountActionF {
  implicit val functor: Functor[AccountActionF] = new Functor[AccountActionF] {
    def map[A,B](action: AccountActionF[A])(f: A => B): AccountActionF[B] = action match {
      case o @ Open(no, name, openingDate, onResult) => o.copy(onResult = onResult andThen f)
      case c @ Close(no, closeDate, onResult)        => c.copy(onResult = onResult andThen f)
      case d @ Debit(no, amount, onResult)           => d.copy(onResult = onResult andThen f)
      case r @ Credit(no, amount, onResult)          => r.copy(onResult = onResult andThen f)
      case b @ GetBalance(no, onResult)              => b.copy(onResult = onResult andThen f)
      case Fails(th)                                 => Fails(th): AccountActionF[B]
    }
  }
}

case class AccountService(name: String) {
  def open(no: String, name: String, openingDate: Option[Date]): AccountAction[Account] = 
    liftF(Open(no, name, openingDate, identity))
  
  def close(no: String, closeDate: Option[Date]): AccountAction[Account] = 
    liftF(Close(no, closeDate, identity))
  
  def debit(no: String, amount: Amount): AccountAction[Account] = 
    liftF(Debit(no, amount, identity))
  
  def credit(no: String, amount: Amount): AccountAction[Account] = 
    liftF(Credit(no, amount, identity))
  
  def balance(no: String): AccountAction[Balance] = 
    liftF(GetBalance(no, identity))
  
  def error[A](e: Throwable) = liftF(Fails(e):AccountActionF[A])
}


