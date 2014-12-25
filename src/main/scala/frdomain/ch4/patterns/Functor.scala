package frdomain.ch4
package patterns

import scala.language.higherKinds

trait Functor[F[_]] {
  def fmap[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] =
    implicitly[Functor[F]]

  implicit def ListFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](a: List[A])(f: A => B): List[B] = a map f
  }

  implicit def OptionFunctor: Functor[Option] = new Functor[Option] {
    def fmap[A, B](a: Option[A])(f: A => B): Option[B] = a.map(f).orElse(None)
  }

  implicit def Tuple2Functor[A1]: Functor[({type f[x] = (A1, x)})#f] = new Functor[({type f[x] = (A1, x)})#f] {
    def fmap[A, B](a: (A1, A))(f: A => B): (A1, B) = (a._1, f(a._2))
  }
}

object FunctorTest {
  import Functor._

  val x = List(1,2,3,4)
  val f: Int => Int = _ + 1

  Functor[List].fmap(x)(f) // List(2,3,4,5)

  val l = List(("a", 10), ("b", 20))
  Functor[List].fmap(l)(t => Functor[({type f[x] = (String, x)})#f].fmap(t)(f)) // List[(String, Int)] = List((a,11), (b,21))

  import Syntax._

  List(1,2,3) fmap f // List(2,3,4)
  l.fmap(e => Functor[({type f[x] = (String, x)})#f].fmap(e)(f))  // List[(String, Int)] = List((a,11), (b,21))

  type Tup[A] = (String, A)
  l.fmap(e => Functor[Tup].fmap(e)(f)) // List[Tup[Int]] = List((a,11), (b,21))
}
