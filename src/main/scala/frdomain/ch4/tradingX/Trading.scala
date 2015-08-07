package frdomain.ch4
package tradingx

import scalaz._
import Scalaz._

trait Trading[Account, Trade, ClientOrder, Order, Execution, Market] {

  def clientOrders: Kleisli[Valid, ClientOrder, Order]
  def execute(market: Market, brokerAccount: Account): Kleisli[Valid, Order, Execution]
  def allocate(accounts: List[Account]): Kleisli[Valid, Execution, Trade]
  def addValueDate: Kleisli[Valid, Trade, Trade]
  def enrich: Kleisli[Valid, Trade, Trade]

  def tradeGeneration(market: Market, broker: Account, clientAccounts: List[Account]) = {
    clientOrders               andThen    
    execute(market, broker)    andThen   
    allocate(clientAccounts)   andThen
    addValueDate               andThen
    enrich
  }
}
