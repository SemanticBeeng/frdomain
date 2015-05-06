package frdomain.ch6
package streams

import scalaz.{ Source => Sourcez, Sink => Sinkz, _ }
import Scalaz._

import akka.actor.ActorSystem
import akka.stream.scaladsl._
import akka.stream._
import scala.language.postfixOps
import scala.concurrent.duration._
import scala.concurrent.Future


import common._
import OnlineService._
import TransactionUnit._

object Main {
  implicit val as = ActorSystem()
  implicit val ec = as.dispatcher
  val settings = ActorFlowMaterializerSettings(as)
  implicit val mat = ActorFlowMaterializer(settings)

  val transactions: Source[Transaction, Unit] =
    Source(allTransactions).mapConcat(identity)

  val amountSink: Sink[TransactionUnit, Future[TransactionUnit]] = 
    Sink.fold[TransactionUnit, TransactionUnit](TransactionUnitMonoid.zero)(_ |+| _)

  val writeTransactions: Sink[Transaction, Future[Unit]] = Sink.foreach(println)

  val totalAmount: RunnableFlow[Future[TransactionUnit]] = 
    transactions.map(_.unit).toMat(amountSink)(Keep.right)
    // transactions.groupBy(_.accountNo).map { case (a, s) => s.map(_.unit).toMat(amountSink)(Keep.right) }
    // 

  val g = FlowGraph.closed() { implicit b =>
    import FlowGraph.Implicits._
 
    val bcast = b.add(Broadcast[Transaction](2))
    transactions ~> bcast.in
    bcast.out(0) ~> Flow[Transaction].map(identity) ~> writeTransactions 
    // change this to update Balance in Account
    bcast.out(1) ~> Flow[Transaction].map(_.unit).toMat(amountSink)(Keep.right)
  }
}
