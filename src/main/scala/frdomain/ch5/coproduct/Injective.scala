package frdomain.ch5
package coproduct

import scalaz._
import Scalaz._

import scala.language.higherKinds

object Injective {

  /** extends Natural Trans with "or" for composite interpreter */
  implicit class RichNat[F[_],G[_]](val nat: F ~> G) extends AnyVal {
    def or[H[_]](f: H ~> G): ({ type f[x] = Coproduct[F, H, x]})#f ~> G =
      new (({type f[x] = Coproduct[F,H,x]})#f ~> G) {
        def apply[A](c: Coproduct[F,H,A]): G[A] = c.run match {
          case -\/(fa) => nat(fa)
          case \/-(ha) => f(ha)
        }
      }
  }

  def lift[F[_], G[_], A](f: F[A])(implicit I: Inject[F, G]): Free.FreeC[G, A] = {
    Free.liftFC(I.inj(f))
  }

}

