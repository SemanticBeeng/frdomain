package frdomain.ch4
package patterns

import scala.language.higherKinds
import java.util.{ Date, Calendar }


object F {
  def accountsOpenedBefore(date: Date): List[Account] = ???
  def accountFor(no: String): Option[Account] = ???
  def interestOn(a: Account): BigDecimal = ???
  def close(a: Account): Account = ???

  def calculateInterest(dt: Date) = accountsOpenedBefore(dt).map(interestOn)
  def calculateInterest(no: String) = accountFor(no).map(interestOn)
  def closeAccount(no: String) = accountFor(no).map(close)

  def fmap[F[_], A, B](fa: F[A])(f: A => B)(implicit ft: Functor[F]) = ft.map(fa)(f) 

  def calculateInterestF(dt: Date): List[BigDecimal] = fmap(accountsOpenedBefore(dt))(interestOn)
  def calculateInterestF(no: String): Option[BigDecimal] = fmap(accountFor(no))(interestOn)
  def closeAccountF(no: String) = fmap(accountFor(no))(close)
}
