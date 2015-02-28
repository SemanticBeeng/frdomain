package frdomain.ch5
package coproduct

import java.util.Date
import scalaz._
import Scalaz._
import Free._

import common._

import scala.language.higherKinds

import Injective._

object Main {
  def prg[F[_]](implicit R: AccountRepository[F], S: AccountService[F]) = {
    import R._; import S._

    type CoyoF[T] = ({type f[x] = Coyoneda[F, x]})#f[T]

    for {
      a <- open("a-123", "debasish ghosh", Some(today))
      b <- credit(a.get, 1000)
      c <- credit(b, 1000)
      d <- debit(c, 500)
      _ <- store(d)
      c <- balance(d)
    } yield (c)
  }

  type App[A] = Coproduct[AccountRepo, AccountAction, A]

  val app: Free.FreeC[App, Balance] = prg[App]

  val interpreters: App ~> Id = AccountRepoInterpreter or AccountServiceInterpreter
  val coyoint: ({type f[x] = Coyoneda[App, x]})#f ~> Id = Coyoneda.liftTF(interpreters)
  def runApp = app.mapSuspension(coyoint)
}
