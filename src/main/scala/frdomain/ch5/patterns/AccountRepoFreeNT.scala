package frdomain.ch5
package patterns

import scalaz._
import Scalaz._
import Free._

object FreeReposNT {
  case class Query[A](no: String, onResult: String \/ Account => A) extends AccountRepoF[A]
  case class Store[A](account: Account, next: A) extends AccountRepoF[A]
  case class Delete[A](no: String, next: A) extends AccountRepoF[A]
  case class Fail[A](e: Throwable) extends AccountRepoF[A]
  
  trait AccountRepository {
  
    implicit val functor: Functor[AccountRepoF] = new Functor[AccountRepoF] {
      def map[A,B](action: AccountRepoF[A])(f: A => B): AccountRepoF[B] = action match {
        case Store(account, next) => Store(account, f(next))
        case Query(no, onResult) => Query(no, onResult andThen f)
        case Delete(no, next) => Delete(no, f(next))
        case Fail(th) => Fail(th): AccountRepoF[B]
      }
    }
  
    def store(account: Account): AccountRepo[Unit] = 
      liftF(Store(account, ()))
    
    def query(no: String): AccountRepo[String \/ Account] = 
      liftF(Query(no, identity))
    
    def delete(no: String): AccountRepo[Unit] = 
      liftF(Delete(no, ()))
  
    def error[A](e: Throwable) = liftF(Fail(e):AccountRepoF[A])
  
    def update(no: String, f: Account => Account): AccountRepo[String \/ Unit] = 
      query(no) flatMap {
        case \/-(a) => 
          for {
            x <- store(f(a))
          } yield x.right

        case -\/(s) => Free.point(s.left)
      }
  }

  object AccountRepoState extends AccountRepository {
    type AMap = Map[String, Account]
    type AccountState[A] = State[AMap, A]
  
    def interpret: AccountRepoF ~> AccountState = new (AccountRepoF ~> AccountState) {
      def apply[A](action: AccountRepoF[A]): AccountState[A] = action match {
        case Query(no, onResult) => 
          State((s: AMap) => s.get(no).map { a => ((s, onResult(a.right))) }.getOrElse((s, onResult(s"No account with $no".left))))
        case Store(account, next) =>
          State((s: AMap) => ((s + (account.no -> account)), next))
        case Delete(no, next) =>
          State((s: AMap) => ((s - no), next))
      }
    }
    val x = Scripts.script.foldMap(interpret)
  }

  object Scripts extends AccountRepository {
    import common._
    val ca = Account("a-123", "debasish ghosh")
    val close: Account => Account = { _.copy(dateOfClosing = Some(today)) }
  
    val script: AccountRepo[String \/ Unit] = for {
      _ <- store(ca)
      a <- update(ca.no, close)
    } yield (a)
  
    val badScript: AccountRepo[Unit] = for {
      _ <- store(ca)
      a <- query("a-123")
      _ <- update("d-123", close)
    } yield ()
  }
}
