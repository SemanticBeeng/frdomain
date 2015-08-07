package frdomain.ch4

import scalaz._
import Scalaz._

package object tradingx {
  type StringOr[A] = String \/ A
  type Valid[A] = ListT[StringOr, A]
}



