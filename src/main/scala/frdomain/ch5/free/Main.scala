package frdomain.ch5
package free

import scalaz._
import Scalaz._
import Free._

object Main {

  import common._
  val ca = Account("a-123", "debasish ghosh")
  val close: Account => Account = { _.copy(dateOfClosing = Some(today)) }

  val db = AccountRepository("ar")
  import db._
  
  val fixture: AccountRepo[Account] = for {
    _ <- store(ca)
    a <- query(ca.no)
    _ <- update(a.no, close)
    b <- query(ca.no)
  } yield (b)

  val badFixture: AccountRepo[Unit] = for {
    _ <- store(ca)
    a <- query(ca.no)
    _ <- update("d-123", close)
  } yield ()

  val t = AccountRepoMutableInterpreter().apply(fixture)
  
  /**
  scala> t.run
  res1: frdomain.ch5.free.Account = Account(a-123,debasish ghosh,Thu Feb 26 00:58:02 GMT+05:30 2015,Some(Thu Feb 26 00:58:02 GMT+05:30 2015),Balance(0))
   **/

  val u = AccountRepoMutableInterpreter().apply(badFixture)
}
