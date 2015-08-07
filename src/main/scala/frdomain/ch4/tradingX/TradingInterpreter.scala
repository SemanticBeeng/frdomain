package frdomain.ch4
package tradingx

import java.util.{ Date, Calendar }
import scalaz.{ Order => OrderZ, _ }
import Scalaz._
import Kleisli._
import ListT._

import TradeModel._

trait TradingInterpreter extends Trading[Account, Trade, ClientOrder, Order, Execution, Market] {

  def clientOrders: Kleisli[Valid, ClientOrder, Order] = kleisli(fromClientOrders)

  def execute(market: Market, brokerAccount: Account) = kleisli[Valid, Order, Execution] { order =>
    println(s"order = $order")
    fromList[StringOr, Execution](
      order.items.map { item =>
        Execution(brokerAccount, item.ins, "e-123", market, item.price, item.qty)
      }.right
    )
  }

  def allocate(accounts: List[Account]) = kleisli[Valid, Execution, Trade] { execution =>
    println(s"execution = $execution")
    val q = execution.quantity / accounts.size
    fromList[StringOr, Trade](
      accounts.map { account =>
        makeTrade(account, execution.instrument, "t-123", execution.market, execution.unitPrice, q)
      }.sequenceU
    )
  }

  def addValueDate = kleisli[Valid, Trade, Trade] { trade =>
    val c = Calendar.getInstance
    c.setTime(trade.tradeDate)
    c.add(Calendar.DAY_OF_MONTH, 3)
    fromList[StringOr, Trade](List(valueDateLens.set(trade, Some(c.getTime))).right)
  }

  def enrich = kleisli[Valid, Trade, Trade] { trade =>
    val taxes = for {
      taxFeeIds      <- forTrade // get the tax/fee ids for a trade
      taxFeeValues   <- taxFeeCalculate // calculate tax fee values
    } yield(taxFeeIds ∘ taxFeeValues)

    val t = taxFeeLens.set(trade, taxes(trade))
    fromList[StringOr, Trade](List(netAmountLens.set(t, t.taxFees.map(_.foldLeft(principal(t))((a, b) => a + b._2)))).right)
  }
}

object TradingInterpreter extends TradingInterpreter

