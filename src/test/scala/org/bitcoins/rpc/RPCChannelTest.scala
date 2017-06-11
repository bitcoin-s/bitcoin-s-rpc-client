package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.channels.{ChannelClient, ChannelServer}
import org.bitcoins.rpc.util.TestUtil
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpec, MustMatchers}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}
/**
  * Created by chris on 5/3/17.
  */
class RPCChannelTest extends FlatSpec with MustMatchers with ScalaFutures
  with BeforeAndAfterAll with BitcoinSLogger {

  implicit val actorSystem = ActorSystem("RPCClientTest")
  implicit val materializer = ActorMaterializer()
  implicit val dispatcher = materializer.system.dispatcher
  //bitcoind -rpcuser=$RPC_USER -rpcpassword=$RPC_PASS -regtest -txindex -daemon

  "RPCChannels" must "create a payment channel from the client's perspective" in {
    val serverAndClient = TestUtil.paymentChannel
    val channelClient = serverAndClient.map(_._1)
    val channelServer = serverAndClient.map(_._2)
    val (serverSPK,_) = ScriptGenerators.p2pkhScriptPubKey.sample.get
    val closed: Future[Transaction] = channelServer.flatMap(_.close(serverSPK))
    val closedConfs = closed.flatMap { tx =>
      //wait for close tx to propogate to client1
      Thread.sleep(2500)
      val genBlocks2 = channelClient.map(_.client.generate(10))
      genBlocks2.flatMap { _ =>
        //time for blocks to propogate
        Thread.sleep(7000)
        channelClient.flatMap(_.client.getConfirmations(tx.txId))
      }
    }
    closedConfs.onComplete( _ => killClients(channelClient, channelServer))
    whenReady(closedConfs, timeout(30.seconds), interval(500.millis)) { confs =>
      confs must be (Some(10))
      //throw confs
    }
  }


  override def afterAll = {
    materializer.shutdown()
  }

  private def killClients(channelClient: Future[ChannelClient], channelServer: Future[ChannelServer]): Unit = {
    channelClient.onComplete {
      case Success(c) => c.client.stop
      case Failure(_) =>
        logger.info("ChannelClient future failed to complete")
        ()
    }

    channelServer.onComplete {
      case Success(c) => c.client.stop
      case Failure(_) =>
        logger.info("ChannelServer future failed to complete")
        ()
    }
  }
}
