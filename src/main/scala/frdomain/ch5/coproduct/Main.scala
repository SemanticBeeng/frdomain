package frdomain.ch5
package coproduct

import java.util.Date
import scalaz._
import Scalaz._
import Free._

import common._

import scala.language.higherKinds

import Injective._
import AccountRepo._

object Main {
  def prg[F[_]](implicit R: AccountRepository[F], S: AccountService[F]) = {
    import R._; import S._

    type CoyoF[T] = ({type f[x] = Coyoneda[F, x]})#f[T]

    for {
      a <- open("a-123", "debasish ghosh", Some(today))
      b <- a.map(credit(_, 1000)).getOrElse(Free.point[CoyoF, Option[Account]](None))
      c <- b.map(credit(_, 1000)).getOrElse(Free.point[CoyoF, Option[Account]](None))
      d <- c.map(debit(_, 500)).getOrElse(Free.point[CoyoF, Option[Account]](None))
      _ <- d.map(store(_)).getOrElse(Free.point[CoyoF, Unit](()))
      e <- d.map(balance(_)).getOrElse(Free.point[CoyoF, Balance](Balance(-100)))
    } yield (e)
  }

  type App[A] = Coproduct[AccountRepo, AccountAction, A]

  val app: Free.FreeC[App, Balance] = prg[App]

  val interpreters: App ~> Id = AccountRepoInterpreter or AccountServiceInterpreter
  val coyoint: ({type f[x] = Coyoneda[App, x]})#f ~> Id = Coyoneda.liftTF(interpreters)
  def runApp = app.mapSuspension(coyoint)
}
