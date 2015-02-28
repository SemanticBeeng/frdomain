package frdomain.ch5
package service

import scalaz._
import Scalaz._
import Free._

object Main {

  import common._

  val s = AccountService("test")
  import s._

  val fixture: AccountAction[Balance] = for {
    a <- open("a-123", "debasish ghosh", Some(today))
    _ <- credit(a.no, 1000)
    _ <- credit(a.no, 3000)
    _ <- debit(a.no, 450)
    b <- balance(a.no)
  } yield (b)

  val t = AccountServiceTaskInterpreter(new AccountMapRepository()).apply(fixture)
}

