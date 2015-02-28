package frdomain.ch5
package patterns

import scala.collection.mutable.{ Map => MMap }
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import Task._
import Free._

trait AccountRepoF[A]
object FreeRepos {
  
  case class Query[A](no: String, onResult: Account => A) extends AccountRepoF[A]
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
    
    def query(no: String): AccountRepo[Account] = 
      liftF(Query(no, identity))
    
    def delete(no: String): AccountRepo[Unit] = 
      liftF(Delete(no, ()))
  
    def error[A](e: Throwable) = liftF(Fail(e):AccountRepoF[A])
  
    def update(no: String, f: Account => Account): AccountRepo[Unit] = for {
      a <- query(no)
      _ <- store(f(a))
    } yield ()
  }
  
  trait AccountRepoInterpreter {
    def apply[A](action: AccountRepo[A]): Task[A]
  }
  
  /**
   * Basic interpreter that uses a global mutable Map to store the state
   * of computation
   */
  case class AccountRepoMutableInterpreter() extends AccountRepoInterpreter with AccountRepository {
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
  
  object AccountRepoPure extends AccountRepository {
    def interpretPure(kvs: AccountRepo[Unit], table: Map[String, Account] = Map.empty): String \/ Map[String, Account] = kvs.resume.fold({
      case Query(no, onResult) => {
        table.get(no).map { a => interpretPure(onResult(a), table) }
                     .getOrElse(s"Account no $no not found".left)
      }
      case Store(account, next) => interpretPure(next, table + (account.no -> account))
      case Delete(no, next) => interpretPure(next, table - no)
      case Fail(th) => th.getMessage.left
    }, _ => table.right)
  }
  
  object AccountRepoState extends AccountRepository {
    type AMap = Map[String, Account]
    type AccountState[A] = State[AMap, A]
  
    def interpret: AccountRepoF ~> AccountState = new (AccountRepoF ~> AccountState) {
      def apply[A](action: AccountRepoF[A]): AccountState[A] = action match {
        case Query(no, onResult) => 
          State((s: AMap) => s.get(no).map { a => ((s, onResult(a))) }.getOrElse(sys.error("No account found")))
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
  
    val script: AccountRepo[Account] = for {
      _ <- store(ca)
      a <- query("a-123")
      _ <- update(a.no, close)
      b <- query("a-123")
    } yield (b)
  
    val badScript: AccountRepo[Unit] = for {
      _ <- store(ca)
      a <- query("a-123")
      _ <- update("d-123", close)
    } yield ()
  }
}
