package frdomain.ch5
package free

import scala.collection.mutable.{ Map => MMap }
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._
import Free._

trait AccountRepoInterpreter {
  def apply[A](action: AccountRepo[A]): Task[A]
}
  
/**
 * Basic interpreter that uses a global mutable Map to store the state
 * of computation
 */
case class AccountRepoMutableInterpreter() extends AccountRepoInterpreter {
  val table: MMap[String, Account] = MMap.empty[String, Account]

  def step[A](action: AccountRepoF[AccountRepo[A]]): Task[AccountRepo[A]] = action match {
    case Fail(e) => fail(e)

    case Query(no, onResult) => 
      table.get(no).map { a => now(onResult(a)) }
                   .getOrElse { fail(new RuntimeException(s"Account no $no not found")) }

    case Store(account, next) => now(table += ((account.no, account))).map { _ => next }
    case Delete(no, next) => now(table - no).map { _ => next }
  }

  /**
   * Turns the AccountRepo script into a `Task` that executes it in a mutable setting
   */
  def apply[A](action: AccountRepo[A]): Task[A] = action.runM(step)
}
