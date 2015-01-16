package frdomain.ch4
package patterns

import scala.language.higherKinds
import java.util.{ Date, Calendar }

/*
sealed trait Currency
case object USD extends Currency
case object JPY extends Currency
case object AUD extends Currency
case object INR extends Currency

object common {
  type Amount = BigDecimal
  val today = Calendar.getInstance.getTime
}

import common._
import Monoid._
import Syntax._

case class Money(m: Map[Currency, Amount]) {
  def toBaseCurrency: Amount = ???
}

case class Balance(amount: Money)

case class Account(no: String, name: String, dateOfOpening: Date = today, dateOfClosing: Option[Date] = None, 
  balance: Balance = Balance(zeroMoney))

object Service {
  private def balanceInBaseCurrency(a: Account) = a.balance.amount.toBaseCurrency

  def gen[F[_]: Functor, A, B](fa: F[A])(f: A => B) = fa map f

  def computeTax(accounts: List[Account])(f: Account => Money): List[Money] = gen(accounts)(f)
  def computeTax(account: Option[Account])(f: Account => Money): Option[Money] = gen(account)(f)
  def inBaseCurrency(accounts: List[Account]): List[Amount] = gen(accounts)(balanceInBaseCurrency)
}

object Foo {

  import Applicative._

  val a1 = Account(no = "a1", name = "a1_name")
  val a2 = Account(no = "a2", name = "a2_name")
  val m = Map("a1" -> a1, "a2" -> a2)

  def foo(fa: String, fb: String) = {
    OptionApply.map2(m.get(fa), m.get(fb)) { (ac1, ac2) => s"$ac1 / $ac2" 
    }
  }
}
*/
