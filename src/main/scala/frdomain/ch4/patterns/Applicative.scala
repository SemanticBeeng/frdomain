package frdomain.ch4
package patterns

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: => A): F[A]
  def ap[A, B](a: F[A])(f: F[A => B]): F[B]
}

object Applicative {
  def apply[F[_]: Applicative]: Applicative[F] =
    implicitly[Applicative[F]]

  implicit def ListApply: Applicative[List] = new Applicative[List] {
    def fmap[A, B](a: List[A])(f: A => B): List[B] = a map f
    def pure[A](a: => A): List[A] = List(a)
    def ap[A, B](as: List[A])(fs: List[A => B]): List[B] = for {
      a <- as
      f <- fs
    } yield f(a)
  }

  implicit def OptionApply: Applicative[Option] = new Applicative[Option] {
    def fmap[A, B](a: Option[A])(f: A => B): Option[B] = a map f
    def pure[A](a: => A): Option[A] = Some(a)
    def ap[A, B](as: Option[A])(fs: Option[A => B]): Option[B] = for {
      a <- as
      f <- fs
    } yield f(a)
  }
}
