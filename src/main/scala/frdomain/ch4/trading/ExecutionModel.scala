package frdomain.ch4
package trading

trait ExecutionModel {this: RefModel with OrderModel =>
  case class Execution(account: Account, instrument: Instrument, refNo: String, market: Market,
    unitPrice: BigDecimal, quantity: BigDecimal)

  trait ExecutionLaw {
    def instrumentsMatchWithOrderLaw: Order => List[Execution] => Boolean = { o => es =>
      o.items.map(_.ins).distinct.size == es.map(_.instrument).distinct.size
    }
  }
}

