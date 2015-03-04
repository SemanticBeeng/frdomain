package frdomain.ch5
package coproduct

import scala.collection.mutable.{ Map => MMap }
import scalaz._
import Scalaz._
import Free._

import scala.language.higherKinds

import Injective._

sealed trait AccountRepo[A]

object AccountRepo {
  type RepoStatus[A] = String \/ A
}

import AccountRepo._
  
case class Query(no: String) extends AccountRepo[RepoStatus[Account]]
case class Store(account: Account) extends AccountRepo[RepoStatus[Unit]]
case class Delete(no: String) extends AccountRepo[RepoStatus[Unit]]

class AccountRepository[F[_]](implicit I: Inject[AccountRepo, F]) {
  def store(account: Account): Free.FreeC[F, RepoStatus[Unit]] = 
    lift(Store(account))
  
  def query(no: String): Free.FreeC[F, RepoStatus[Account]] =
    lift(Query(no))
  
  def delete(no: String): Free.FreeC[F, RepoStatus[Unit]] =
    lift(Delete(no))
}

object AccountRepository {
  implicit def instance[F[_]](implicit I: Inject[AccountRepo, F]): AccountRepository[F] = new AccountRepository[F]
}

object AccountRepoInterpreter extends (AccountRepo ~> Id) {
  val table: MMap[String, Account] = MMap.empty[String, Account]

  def apply[A](r: AccountRepo[A]) = r match {
    case Query(no) => table.get(no).map(_.right).getOrElse(s"Account $no not found".left)
    case Store(a) => {
      table += ((a.no, a))
      ().right
    }
    case Delete(no) => {
      table -= no
      ().right
    }
  }
}
