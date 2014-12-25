package frdomain.ch4
package patterns

import scala.language.higherKinds

object Syntax {
  implicit class FunctorSyntax[F[_]: Functor, A](a: F[A]) {
    def fmap[B](f: A => B) = Functor[F].fmap(a)(f)
  }
}


