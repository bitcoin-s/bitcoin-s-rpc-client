package org.bitcoins.rpc.channels

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.BitcoinSLogger
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
  with BeforeAndAfterAll {
  private val logger = BitcoinSLogger.logger
  implicit val actorSystem = ActorSystem("RPCClientTest")
  implicit val materializer = ActorMaterializer()
  implicit val dispatcher = materializer.system.dispatcher
  //bitcoind -rpcuser=$RPC_USER -rpcpassword=$RPC_PASS -regtest -txindex -daemon

  "RPCChannels" must "create a payment channel and close it via the escrow branch of the contract" in {
    val clientAndServer = TestUtil.initializedChannel
    val channelClient = clientAndServer.map(_._1)
    val channelServer = clientAndServer.map(_._2)
    val (serverSPK,_) = ScriptGenerators.p2pkhScriptPubKey.sample.get
    val closed: Future[Transaction] = channelServer.flatMap(_.close(serverSPK))
    val closedConfs = closed.flatMap { tx =>
      //wait for close tx to propogate to client1
      Thread.sleep(2500)
      val genBlocks2 = channelClient.map(_.client.generate(10))
      genBlocks2.flatMap { _ =>
        //time for blocks to propogate
        Thread.sleep(12500)
        channelClient.flatMap(_.client.getConfirmations(tx.txId))
      }
    }
    closedConfs.onComplete(_ => killClients(channelClient, channelServer))
    whenReady(closedConfs, timeout(45.seconds), interval(500.millis)) { confs =>
      confs must be (Some(10))
    }
  }

  it must "create a payment channel and close it via the timeout branch of a contract" in {
    val clientAndServer = TestUtil.initializedChannel
    val channelClient = clientAndServer.map(_._1)
    val channelServer = clientAndServer.map(_._2)

    val generateBlocksToHaveTimeLockExpire: Future[Seq[DoubleSha256Digest]] = channelClient.flatMap(_.client.generate(90))
    val closed = generateBlocksToHaveTimeLockExpire.flatMap { _ =>
      channelClient.flatMap(_.closeWithTimeout)
    }
    val generateBlocksToConfirm = closed.flatMap { c => channelClient.flatMap(_.client.generate(10)) }
    val closedConfs = generateBlocksToConfirm.flatMap { _ =>
      closed.flatMap { tx =>
        Thread.sleep(5000)
        channelServer.flatMap(_.client.getConfirmations(tx.txId))
      }
    }
    closedConfs.onComplete(_ => killClients(channelClient,channelServer))
    whenReady(closedConfs,timeout(45.seconds), interval(500.millis)) { confs =>
      confs must be (Some(10))
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
