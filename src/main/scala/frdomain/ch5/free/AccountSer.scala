package frdomain.ch5
package free

import java.util.Date
import util.{ Try, Success, Failure }
import scalaz._
import Scalaz._

trait AccountSer[Account, Amount, Balance] {
  def open(no: String, name: String, openingDate: Option[Date]): Reader[AccountRepository, Try[Account]]
  def close(no: String, closeDate: Option[Date]): Reader[AccountRepository, Try[Account]]
  def debit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]]
  def credit(no: String, amount: Amount): Reader[AccountRepository, Try[Account]]
  def balance(no: String): Reader[AccountRepository, Try[Balance]]
}

