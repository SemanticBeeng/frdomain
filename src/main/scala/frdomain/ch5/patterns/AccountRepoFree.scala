package frdomain.ch5
package patterns

import scalaz._
import Scalaz._
import Free._

sealed trait AccountRepoF[A]

case class Query[A](no: String, onResult: Account => A) extends AccountRepoF[A]
case class Store[A](account: Account, next: A) extends AccountRepoF[A]
case class Delete[A](no: String, next: A) extends AccountRepoF[A]

trait AccountRepository {
  type AccountRepo[A] = Free[AccountRepoF, A]

  implicit val functor: Functor[AccountRepoF] = new Functor[AccountRepoF] {
    def map[A,B](action: AccountRepoF[A])(f: A => B): AccountRepoF[B] = action match {
      case Store(account, next) => Store(account, f(next))
      case Query(no, onResult) => Query(no, onResult andThen f)
      case Delete(no, next) => Delete(no, f(next))
    }
  }

  def store(account: Account): AccountRepo[Unit] = 
    liftF(Store(account, ()))
  
  def query(no: String): AccountRepo[Account] = 
    liftF(Query(no, identity))
  
  def delete(no: String): AccountRepo[Unit] = 
    liftF(Delete(no, ()))

  def update(no: String, f: Account => Account): AccountRepo[Unit] = for {
    a <- query(no)
    _ <- store(f(a))
  } yield ()
}

object Scripts extends AccountRepository {
  import common._
  val ca = Account("a-123", "debasish ghosh")
  val close: Account => Account = { _.copy(dateOfClosing = Some(today)) }

  val script: AccountRepo[Unit] = for {
    _ <- store(ca)
    a <- query("a-123")
    _ <- update(a.no, close)
  } yield ()

  val badScript: AccountRepo[Unit] = for {
    _ <- store(ca)
    a <- query("a-123")
    _ <- update("d-123", close)
  } yield ()
}

object AccountRepoPure extends AccountRepository {
  def interpretPure(kvs: AccountRepo[Unit], table: Map[String, Account] = Map.empty): String \/ Map[String, Account] = kvs.resume.fold({
    case Query(no, onResult) => {
      table.get(no).map { a => interpretPure(onResult(a), table) }
                   .getOrElse(s"Account no $no not found".left)
    }
    case Store(account, next) => interpretPure(next, table + (account.no -> account))
    case Delete(no, next) => interpretPure(next, table - no)
  }, _ => table.right)
}
