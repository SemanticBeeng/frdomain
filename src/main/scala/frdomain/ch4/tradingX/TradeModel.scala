package frdomain.ch4
package tradingx

import java.util.{ Date, Calendar }

import scalaz._
import Scalaz._
import \/._
import Lens._

trait TradeModel {this: RefModel =>

  case class Trade private[tradingx] (account: Account, instrument: Instrument, refNo: String, market: Market,
    unitPrice: BigDecimal, quantity: BigDecimal, tradeDate: Date = today,
    valueDate: Option[Date] = None, taxFees: Option[List[(TaxFeeId, BigDecimal)]] = None, 
    netAmount: Option[BigDecimal] = None)

  sealed trait TaxFeeId
  case object TradeTax extends TaxFeeId
  case object Commission extends TaxFeeId
  case object VAT extends TaxFeeId
  case object Surcharge extends TaxFeeId

  // rates of tax/fees expressed as fractions of the principal of the trade
  val rates: Map[TaxFeeId, BigDecimal] = Map(TradeTax -> 0.2, Commission -> 0.15, VAT -> 0.1)

  // tax and fees applicable for each market
  // Other signifies the general rule
  val taxFeeForMarket: Map[Market, List[TaxFeeId]] = 
    Map(Other -> List(TradeTax, Commission), Singapore -> List(TradeTax, Commission, VAT))

  // get the list of tax/fees applicable for this trade
  // depends on the market
  val forTrade: Trade => Option[List[TaxFeeId]] = {trade =>
    taxFeeForMarket.get(trade.market).orElse(taxFeeForMarket.get(Other)) 
  }

  def principal(trade: Trade) = trade.unitPrice * trade.quantity

  // combinator to value a tax/fee for a specific trade
  private val valueAs: Trade => TaxFeeId => BigDecimal = {trade => {tid =>
    ((rates get tid) map (_ * principal(trade))) getOrElse (BigDecimal(0)) }}

  // all tax/fees for a specific trade
  val taxFeeCalculate: Trade => List[TaxFeeId] => List[(TaxFeeId, BigDecimal)] = {t => tids =>
    tids zip (tids map valueAs(t))
  }

  type ReaderTStatus[A, S] = ReaderT[StringOr, A, S]

  object ReaderTStatus extends KleisliInstances with KleisliFunctions {
    def apply[A, S](f: A => StringOr[S]): ReaderTStatus[A, S] = kleisli(f)
  }

  private def validQuantity = ReaderTStatus[Trade, Trade] { trade =>
    if (trade.quantity < 0) left(s"Quantity needs to be > 0 for $trade") else right(trade)
  }

  private def validUnitPrice = ReaderTStatus[Trade, Trade] { trade =>
    if (trade.unitPrice < 0) left(s"Unit Price needs to be > 0 for $trade") else right(trade)
  }

  private def validValueDate = ReaderTStatus[Trade, Trade] { trade =>
    trade.valueDate.map(vd => 
      if (trade.tradeDate after vd) left(s"Trade Date ${trade.tradeDate} must be before value date $vd")
      else right(trade)).getOrElse(right(trade))
  }

  def makeTrade(account: Account, 
    instrument: Instrument, 
    refNo: String, 
    market: Market, 
    unitPrice: BigDecimal, 
    quantity: BigDecimal,
    td: Date = today, 
    vd: Option[Date] = None): StringOr[Trade] = {

    val trd = Trade(account, instrument, refNo, market, unitPrice, quantity, td, vd)
    val s = for {
      _ <- validQuantity
      _ <- validValueDate
      t <- validUnitPrice
    } yield t
    s(trd)
  }

  /**
   * define a set of lenses for functional updation
   */

  // change ref no
  val refNoLens: Lens[Trade, String] = lensu(
      (t: Trade, r: String) => t.copy(refNo = r),
      (t: Trade) => t.refNo)

  // add tax/fees
  val taxFeeLens: Lens[Trade, Option[List[(TaxFeeId, BigDecimal)]]] = 
    lensu(
      (t: Trade, tfs: Option[List[(TaxFeeId, BigDecimal)]]) => t.copy(taxFees = tfs),
      (t: Trade) => t.taxFees) 

  // add net amount
  val netAmountLens: Lens[Trade, Option[BigDecimal]] = 
    lensu(
      (t: Trade, n: Option[BigDecimal]) => t.copy(netAmount = n),
      (t: Trade) => t.netAmount) 

  // add value date
  val valueDateLens: Lens[Trade, Option[Date]] = 
    lensu(
      (t: Trade, d: Option[Date]) => t.copy(valueDate = d),
      (t: Trade) => t.valueDate)

}

object TradeModel extends ExecutionModel with OrderModel with RefModel with TradeModel 
