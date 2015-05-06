package frdomain.ch6
package streams

import java.util.Date
import scala.concurrent.duration._
import scala.concurrent.{ Future, ExecutionContext }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.immutable._

import scalaz._
import Scalaz._

import common._

sealed trait TransactionType
case object Debit extends TransactionType
case object Credit extends TransactionType

case class TransactionUnit(debitCredit: TransactionType, amount: Amount)

object TransactionUnit {
  implicit val TransactionUnitMonoid = new Monoid[TransactionUnit] {
    val zero = TransactionUnit(Debit, 0)
    def append(i: TransactionUnit, j: => TransactionUnit) = {
      val f = if (i.debitCredit == Debit) -i.amount else i.amount
      val s = if (j.debitCredit == Debit) -j.amount else j.amount
      val sum = f + s
      if (sum < 0) TransactionUnit(Debit, -sum) else TransactionUnit(Credit, sum)
    }
  }
}

case class Transaction(id: String, accountNo: String, unit: TransactionUnit, date: Date = today)

trait OnlineService {
  def allAccounts(implicit ec: ExecutionContext): Future[Seq[String]] = Future {
    Seq("a-1", "a-2", "a-3")
  }

  def allTransactions(implicit ec: ExecutionContext): Future[Seq[Transaction]] = Future {
    Seq(
      Transaction("t-1", "a-1", TransactionUnit(Debit, 1000)),
      Transaction("t-2", "a-2", TransactionUnit(Debit, 1000)),
      Transaction("t-3", "a-3", TransactionUnit(Credit, 1000)),
      Transaction("t-4", "a-1", TransactionUnit(Credit, 1000)),
      Transaction("t-5", "a-1", TransactionUnit(Debit, 1000)),
      Transaction("t-6", "a-2", TransactionUnit(Debit, 1000)),
      Transaction("t-7", "a-3", TransactionUnit(Credit, 1000)),
      Transaction("t-8", "a-3", TransactionUnit(Debit, 1000)),
      Transaction("t-9", "a-2", TransactionUnit(Credit, 1000)),
      Transaction("t-10", "a-2", TransactionUnit(Debit, 1000)),
      Transaction("t-11", "a-1", TransactionUnit(Credit, 1000)),
      Transaction("t-12", "a-3", TransactionUnit(Debit, 1000))
    )
  }
}

object OnlineService extends OnlineService
