package frdomain.ch5
package free

import scalaz._
import Scalaz._
import Free._

object Main {

  import common._
  import AccountService._

  val composite =
    for {
      x <- open("a-123", "debasish ghosh", Some(today))
      _ <- credit(x.no, 10000)
      _ <- credit(x.no, 30000)
      _ <- debit(x.no, 23000)
      a <- query(x.no)
    } yield a

  val t = AccountRepoMutableInterpreter().apply(composite)
}
