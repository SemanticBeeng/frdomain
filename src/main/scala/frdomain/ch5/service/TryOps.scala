package frdomain.ch5
package service

import scala.util.{ Try, Success, Failure }
import scala.language.higherKinds

import scalaz.{ Applicative, Traverse, Monad }
import scalaz.concurrent.Task
import Task._

trait TryOps {

  import scala.language.implicitConversions
  protected implicit class RichTry[+A](t: Try[A]) {
    def toTask: Task[A] = t match {
      case Success(a) => now(a)
      case Failure(e) => fail(e)
    }
  }

  protected implicit val tryInstance = new Monad[Try] with Traverse[Try] {
    def point[A](a: => A) = Success(a)

    def bind[A, B](fa: Try[A])(f: (A) => Try[B]) = fa.flatMap { f }

    def traverseImpl[G[_], A, B](fa: Try[A])(f: (A) => G[B])(implicit Ap: Applicative[G]) = fa match {
      case Success(a) => Ap(f(a)) { point(_) }
      case Failure(e) => Ap.point { Failure(e) }
    }
  }

}
