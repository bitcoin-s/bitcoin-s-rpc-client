package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.protocol.script.{EscrowTimeoutScriptPubKey, MultiSignatureScriptPubKey}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionOutput, WitnessTransaction}
import org.bitcoins.rpc.util.TestUtil
import org.scalatest.{BeforeAndAfterAll, FlatSpec, MustMatchers}
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
/**
  * Created by chris on 5/3/17.
  */
class RPCPaymentChannelTest extends FlatSpec with MustMatchers with ScalaFutures with BeforeAndAfterAll {

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
    val connected = client1.addNode(client1.instance.uri).map { _ =>
      client2.addNode(client2.instance.uri)
    }
    val generateBlocks = connected.flatMap(_ => client1.generate(101))
    Await.result(generateBlocks,5.seconds)
  }

  "RPCPaymentChannels" must "create a payment channel from the client's perspective" in {
    val privKey1 = ECPrivateKey()
    val privKey2 = ECPrivateKey()
    val lockTimeScriptPubKey = ScriptGenerators.lockTimeScriptPubKey.sample.get._1
    val multiSig = MultiSignatureScriptPubKey(2,Seq(privKey1.publicKey, privKey2.publicKey))
    val scriptPubKey = EscrowTimeoutScriptPubKey(multiSig,lockTimeScriptPubKey)
    val output = TransactionOutput(CurrencyUnits.oneBTC, scriptPubKey)
    val unfunded = Transaction(TransactionConstants.version,Nil,Seq(output), TransactionConstants.lockTime)
    val fundedTx: Future[(Transaction, CurrencyUnit, Int)] = client1.fundRawTransaction(unfunded)
    val signedTx = fundedTx.flatMap(f => client1.signRawTransaction(f._1))
    val sent = signedTx.flatMap(tx => client1.sendRawTransaction(tx._1))

  }

  override def afterAll = {
    materializer.shutdown()
    Await.result(TestUtil.stopNodes(Seq(client1,client2)),5.seconds)
  }
}
