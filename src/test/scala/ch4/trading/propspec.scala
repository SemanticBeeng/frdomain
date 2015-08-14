package frdomain.ch4
package trading

import java.util.{ Date, Calendar }

import scala.util.{ Try, Success, Failure }
import org.scalacheck._
import Prop.{ forAll, BooleanOperators }
import Gen._
import Arbitrary.arbitrary

import TradeModel._
import TradingInterpreter._

object TradingSpecification extends Properties("trading") with OrderLaw {

  implicit val arbitraryClientOrder: Arbitrary[ClientOrder] = Arbitrary {
    Gen.oneOf(
      Map("no" -> "o-123", "customer" -> "chase", "instrument" -> "goog/100/30-ibm/200/12"),
      Map("no" -> "o-124", "customer" -> "nomura", "instrument" -> "cisco/100/30-oracle/200/12")
    )
  }

  property("Generated orders must have same number of line items as the number of instruments ordered by client") = 
    forAll((cos: Set[ClientOrder]) => { 

      val orders = for {
        os <- clientOrders.run(cos.toList)
      } yield os

      sizeLaw(cos.toSeq)(orders) == true
      lineItemLaw(cos.toSeq)(orders) == true
    })
}
