package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.stream.ActorMaterializer
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionOutput}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.bitcoincore.networking.{AddedNodeInfo, NodeAddress, OutboundConnection}
import org.bitcoins.rpc.bitcoincore.wallet.WalletTransaction
import org.bitcoins.rpc.util.TestUtil
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpec, MustMatchers}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/**
  * Created by tom on 4/26/16.
  */
class RPCClientTest extends FlatSpec with MustMatchers with ScalaFutures with
  BeforeAndAfterAll with BitcoinSLogger {
  implicit val actorSystem = ActorSystem("RPCClientTest")
  val materializer = ActorMaterializer()
  implicit val dispatcher = materializer.system.dispatcher
  val instance = TestUtil.instance(TestUtil.network.port,TestUtil.network.rpcPort)
  val test = RPCClient(instance,materializer)

  val instance1 = TestUtil.instance(TestUtil.network.port - 10,TestUtil.network.rpcPort - 10)
  val test1 = RPCClient(instance1,materializer)
  //bitcoind -rpcuser=$RPC_USER -rpcpassword=$RPC_PASS -regtest -txindex -daemon

  override def beforeAll: Unit = {
    TestUtil.startNodes(Seq(test,test1))
  }

  "ScalaRPCClient" must "send a command to the command line and return the output" in {
    val hashes = test.generate(101)
    val blockCount = hashes.flatMap(_ => test.getBlockCount)
    whenReady(blockCount, timeout(5.seconds), interval(500.millis)) { count =>
      count must be (101)
    }
  }

  it must "get difficuluty" in {
    whenReady(test.getDifficulty, timeout(5.seconds), interval(500.millis)) { diff =>
      diff must be (4.656542373906925E-10)
    }
  }

  it must "get new address" in {
    whenReady(test.getNewAddress, timeout(5.seconds), interval(500.millis)) { addr =>
      //just make sure we can parse it
    }
  }

  it must "get raw change address" in {
    whenReady(test.getRawChangeAddress, timeout(5.seconds), interval(500.millis)) { addr =>
      //just make sure we can parse it
    }
  }

  it must "get the balance" in {
    whenReady(test.getBalance, timeout(5.seconds), interval(500.millis)) { satoshis =>
      satoshis must be (Bitcoins(50))
    }
  }

  it must "list utxos" in {
    whenReady(test.listUnspent, timeout(5.seconds), interval(500.millis)) { utxos =>
      utxos.nonEmpty must be (true)
    }
  }

  it must "fund a raw transaction" in {
    val output = TransactionOutput(CurrencyUnits.oneBTC,EmptyScriptPubKey)
    val unfunded = Transaction(TransactionConstants.version,Nil,Seq(output),TransactionConstants.lockTime)
    whenReady(test.fundRawTransaction(unfunded), timeout(5.seconds), interval(500.millis)) { case (tx,fee,changepos) =>
      tx.inputs.nonEmpty must be (true)
    }
  }

  it must "be able to import a private key and then dump it" in {
    val key = ECPrivateKey()
    val address = P2PKHAddress(key.publicKey,TestUtil.network)
    val imp = test.importPrivateKey(key)
    val dumpedKeyFuture = imp.flatMap(_ => test.dumpPrivateKey(address))
    whenReady(dumpedKeyFuture, timeout(5.seconds), interval(500.millis)) { dumpedKey =>
      dumpedKey must be (key)
    }
  }

  it must "send a raw transaction to the network" in {
    val signed = generatedTx
    val sent = signed.flatMap(s => test.sendRawTransaction(s))
    val getrawtx = sent.flatMap(s => test.getRawTransaction(s))
    val allInfo: Future[(Transaction,Transaction)] = signed.flatMap { s =>
      getrawtx.map(tx => (s,tx))
    }
    whenReady(allInfo, timeout(5.seconds), interval(5.millis)) { info =>
      info._1 must be (info._2)
    }
  }

  it must "get a transaction from the network" in {
    val signed = generatedTx
    val sent = signed.flatMap(tx => test.sendRawTransaction(tx))
    val getTx = sent.flatMap(txId => test.getTransaction(txId))
    val allInfo: Future[(Transaction,WalletTransaction)] = getTx.flatMap { gTx =>
      signed.map(s => (s,gTx))
    }
    whenReady(allInfo, timeout(5.seconds), interval(500.millis)) { info =>
      info._1 must be (info._2.transaction)
    }
  }

  it must "return an error message for a random command bitcoind does not support" in {
    val f = test.sendCommand("SDFSDFKLDJ:S")
    whenReady(f.failed,timeout(5.seconds),interval(500.millis)) { err =>
      err.isInstanceOf[IllegalArgumentException] must be (true)
    }
  }

  it must "add a node, get the nodes info, then disconnect the node" in {
    val added: Future[Unit] = test.addNode(test1.instance.uri)
    val getInfo = added.flatMap { _ =>
      Thread.sleep(2500)
      test.getAddedNodeInfo
    }
    val disconnect = getInfo.flatMap(_ => test.disconnectNode(test1.instance.uri))
    val getInfo2 = disconnect.flatMap { _ =>
      Thread.sleep(2500)
      test.getAddedNodeInfo
    }
    val result: Future[(Seq[AddedNodeInfo], Seq[AddedNodeInfo])] = getInfo.flatMap { g =>
      getInfo2.map(g2 => (g,g2))
    }
    whenReady(result, timeout(10.seconds), interval(5.millis)) { case (uris,disconnectedUris) =>
      uris.size must be (1)
      uris.head must be (AddedNodeInfo(Uri("localhost:18434"), true,
        Seq(NodeAddress(Uri("/127.0.0.1:18434"), OutboundConnection))))
      //after we disconnect the list must be empty
      disconnectedUris.exists(_.connected) must be (false)
    }
  }

  def generatedTx: Future[Transaction] = {
    val scriptPubKey = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val amount = CurrencyUnits.oneBTC
    val output = TransactionOutput(amount, scriptPubKey)
    val tx = Transaction(TransactionConstants.version,Nil,Seq(output), TransactionConstants.lockTime)
    val generateBlocks = test.generate(101)
    val funded: Future[(Transaction, CurrencyUnit, Int)] = generateBlocks.flatMap(_ => test.fundRawTransaction(tx))
    val signed: Future[(Transaction,Boolean)] = funded.flatMap(f => test.signRawTransaction(f._1))
    signed.map(_._1)
  }
  override def afterAll = {
    Await.result(TestUtil.stopNodes(Seq(test,test1)), 5.seconds)
    materializer.shutdown()
  }
}
