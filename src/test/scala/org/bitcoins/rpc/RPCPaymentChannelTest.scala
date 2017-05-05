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
  val test = RPCClient(TestUtil.instance,materializer)
  //bitcoind -rpcuser=$RPC_USER -rpcpassword=$RPC_PASS -regtest -txindex -daemon

  override def beforeAll: Unit = {
    test.start
    Thread.sleep(20000)
    Await.result(test.generate(101),5.seconds)
  }

  "RPCPaymentChannels" must "create a payment channel from the client's perspective" in {
    val privKey1 = ECPrivateKey()
    val privKey2 = ECPrivateKey()
    val lockTimeScriptPubKey = ScriptGenerators.lockTimeScriptPubKey.sample.get._1
    val multiSig = MultiSignatureScriptPubKey(2,Seq(privKey1.publicKey, privKey2.publicKey))
    val scriptPubKey = EscrowTimeoutScriptPubKey(multiSig,lockTimeScriptPubKey)
    val output = TransactionOutput(CurrencyUnits.oneBTC, scriptPubKey)
    val unfunded = Transaction(TransactionConstants.version,Nil,Seq(output), TransactionConstants.lockTime)
    val fundedTx: Future[(Transaction, CurrencyUnit, Int)] = test.fundRawTransaction(unfunded)
    val signedTx = fundedTx.flatMap(f => test.signRawTransaction(f._1))
    val sent = signedTx.flatMap(tx => test.sendRawTransaction(tx._1))

  }

  override def afterAll = {
    materializer.shutdown()
    Await.result(test.stop,5.seconds)
  }
}
