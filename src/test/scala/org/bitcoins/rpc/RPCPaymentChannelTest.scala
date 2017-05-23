package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.channels.{AnchorTransaction, PaymentChannelAwaitingAnchorTx, PaymentChannelInProgress}
import org.bitcoins.core.crypto.{ECDigitalSignature, ECPrivateKey}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.number.Int64
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{EscrowTimeoutScriptPubKey, MultiSignatureScriptPubKey, WitnessScriptPubKeyV0}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionOutput, WitnessTransaction}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.channels.{PaymentChannelClient, PaymentChannelServer}
import org.bitcoins.rpc.util.TestUtil
import org.scalatest.{BeforeAndAfterAll, FlatSpec, MustMatchers}
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.util.Try
/**
  * Created by chris on 5/3/17.
  */
class RPCPaymentChannelTest extends FlatSpec with MustMatchers with ScalaFutures
  with BeforeAndAfterAll with BitcoinSLogger {

  implicit val actorSystem = ActorSystem("RPCClientTest")
  val materializer = ActorMaterializer()
  implicit val dispatcher = materializer.system.dispatcher
  val node1 = TestUtil.instance(TestUtil.network.port - 15, TestUtil.network.rpcPort - 15)
  val client1 = RPCClient(node1,materializer)
  val node2 = TestUtil.instance(TestUtil.network.port + 15, TestUtil.network.rpcPort + 15)
  val client2 = RPCClient(node2,materializer)
  //bitcoind -rpcuser=$RPC_USER -rpcpassword=$RPC_PASS -regtest -txindex -daemon

  override def beforeAll: Unit = {
    TestUtil.startNodes(Seq(client1,client2))
    val connected = client1.addNode(client2.instance.uri).flatMap { _ =>
      val u = client2.addNode(client1.instance.uri)
      val generateBlocks = u.flatMap(_ => client1.generate(300))
      Thread.sleep(5000)
      generateBlocks
    }

    val generateBlocksClient2 = connected.flatMap(_ => client2.generate(175))
    Await.result(generateBlocksClient2,10.seconds)
  }

  "RPCPaymentChannels" must "create a payment channel from the client's perspective" in {
    val serverPrivKey = ECPrivateKey()
    val serverPubKey = serverPrivKey.publicKey
    val (lockTimeScriptPubKey,_) = ScriptGenerators.lockTimeScriptPubKey.sample.get
    val clientSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val serverSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1

    val pcClient: Future[PaymentChannelClient] = PaymentChannelClient(client1,serverPubKey,
      lockTimeScriptPubKey,CurrencyUnits.oneBTC)
    val generatedBlocks = pcClient.flatMap { _ =>
      Thread.sleep(5000)
      client1.generate(10)
    }

    val pcServer: Future[PaymentChannelServer] = generatedBlocks.flatMap { _ =>
      pcClient.flatMap { cli =>
        //wait for client1 to propogate tx to client2
        Thread.sleep(5000)
        PaymentChannelServer(client2, cli.channel.anchorTx.tx.txId, cli.channel.lock)
      }
    }
    val amount = Policy.minPaymentChannelAmount
    val clientSigned: Future[(PaymentChannelClient,Transaction)] = generatedBlocks.flatMap { _ =>
      pcClient.flatMap(pc => pc.update(clientSPK, amount))
    }

    val pcServerUpdated: Future[PaymentChannelServer] = clientSigned.flatMap { cli =>
      pcServer.flatMap { server =>
        server.update(cli._2,serverPrivKey)
      }
    }

    val closed: Future[Transaction] = pcServerUpdated.flatMap(_.close(serverSPK,serverPrivKey))

    val closedConfs = closed.flatMap { tx =>
      Thread.sleep(10000)
      val genBlocks2 = client1.generate(10)
      genBlocks2.flatMap(_ => client1.getConfirmations(tx.txId))
    }
    whenReady(closedConfs, timeout(30.seconds), interval(500.millis)) { confs =>
      confs must be (Some(10))
    }
  }

  override def afterAll = {
    materializer.shutdown()
    Await.result(TestUtil.stopNodes(Seq(client1,client2)),5.seconds)
  }
}
