package frdomain.ch5
package coproduct

import scala.collection.mutable.{ Map => MMap }
import scalaz._
import Scalaz._
import Free._

import scala.language.higherKinds

import Injective._

sealed trait AccountRepo[A]
  
case class Query(no: String) extends AccountRepo[Option[Account]]
case class Store(account: Account) extends AccountRepo[Unit]
case class Delete(no: String) extends AccountRepo[Unit]

class AccountRepository[F[_]](implicit I: Inject[AccountRepo, F]) {
  def store(account: Account): Free.FreeC[F, Unit] = 
    lift(Store(account))
  
  def query(no: String): Free.FreeC[F, Option[Account]] =
    lift(Query(no))
  
  def delete(no: String): Free.FreeC[F, Unit] =
    lift(Delete(no))
}

object AccountRepository {
  implicit def instance[F[_]](implicit I: Inject[AccountRepo, F]): AccountRepository[F] = new AccountRepository[F]
}

object AccountRepoInterpreter extends (AccountRepo ~> Id) {
  val table: MMap[String, Account] = MMap.empty[String, Account]

  def apply[A](r: AccountRepo[A]) = r match {
    case Query(no) => table.get(no)
    case Store(a) => {
      table += ((a.no, a))
      ()
    }
    case Delete(no) => {
      table -= no
      ()
    }
  }
}
