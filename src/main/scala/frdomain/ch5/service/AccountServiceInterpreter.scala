package frdomain.ch5
package service

import scala.util.{ Try, Success, Failure }
import scalaz.{ Success => Succezz, Failure => Failurez, _ }
import Scalaz._
import scalaz.concurrent.Task
import Task._
import Free._

import common._

trait AccountServiceInterpreter {
  def apply[A](action: AccountAction[A]): Task[A]
}
  
/**
 * Basic interpreter that uses a global mutable Map to store the state
 * of computation
 */
case class AccountServiceTaskInterpreter(ar: AccountRepository) extends AccountServiceInterpreter with TryOps {

  def step[A](action: AccountActionF[AccountAction[A]]): Task[AccountAction[A]] = action match {
    case Fails(e) => fail(e)

    case Open(no, name, odate, onResult) => 
      ar.store(Account(no, name, odate.map(identity).getOrElse(today))).map(onResult(_)).toTask

    case Close(no, cdate, onResult) => 
      val cd = cdate.map(identity).orElse(Some(today))
      ar.query(no) match { 
        case Success(Some(a)) => ar.store(a.copy(dateOfClosing = cd)).map(onResult(_)).toTask
        case Success(None) => fail(new RuntimeException(s"Account no $no not found"))
        case Failure(ex) => fail(ex)
      } 

    case Debit(no, amount, onResult) => 
      ar.query(no) match {
        case Success(Some(a)) if a.balance.amount < amount => fail(new Exception("Insufficient balance"))
        case Success(Some(a)) => ar.store(a.copy(balance = Balance(a.balance.amount - amount))).map(onResult(_)).toTask
        case Success(None) => fail(new Exception(s"Account not found with $no"))
        case Failure(ex) => fail(ex)
      }

    case Credit(no, amount, onResult) => 
      ar.query(no) match {
        case Success(Some(a)) => ar.store(a.copy(balance = Balance(a.balance.amount + amount))).map(onResult(_)).toTask
        case Success(None) => fail(new Exception(s"Account not found with $no"))
        case Failure(ex) => fail(ex)
      }

    case GetBalance(no, onResult) => ar.balance(no).map(onResult(_)).toTask
  }

  /**
   * Turns the AccountRepo script into a `Task` that executes it in a mutable setting
   */
  def apply[A](action: AccountAction[A]): Task[A] = action.runM(step)
}

