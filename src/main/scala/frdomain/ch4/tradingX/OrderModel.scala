package frdomain.ch4
package tradingx

import java.util.{Date, Calendar}

import scalaz._
import Scalaz._
import ListT._

trait OrderModel {this: RefModel =>
  case class LineItem(ins: Instrument, qty: BigDecimal, price: BigDecimal)
  case class Order(no: String, date: Date, customer: Customer, items: List[LineItem])

  case class ClientOrder(details: Map[String, String])

  def fromClientOrders: ClientOrder => Valid[Order] = { cos =>
    fromList[StringOr, Order](List(Order("123", today, "abc", List.empty[LineItem])).right)
  }
}
