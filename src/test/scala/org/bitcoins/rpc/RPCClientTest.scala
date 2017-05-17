package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.stream.ActorMaterializer
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey}
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.{P2PKHAddress, P2SHAddress}
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, P2SHScriptPubKey}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionOutput}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}
import org.bitcoins.rpc.bitcoincore.networking.{AddedNodeInfo, NodeAddress, OutboundConnection}
import org.bitcoins.rpc.bitcoincore.wallet.{FundRawTransactionOptions, ImportMultiRequest, WalletTransaction}
import org.bitcoins.core.util.BitcoinSLogger
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

  it must "estimate a fee for inclusion in a block" in {
    whenReady(test.estimateFee(10), timeout(5.seconds), interval(500.millis)) { fee =>
      fee must be (Satoshis(Int64(50)))
    }
  }

  it must "fund a raw transaction" in {
    val output = TransactionOutput(CurrencyUnits.oneBTC,EmptyScriptPubKey)
    val unfunded = Transaction(TransactionConstants.version,Nil,Seq(output),TransactionConstants.lockTime)
    whenReady(test.fundRawTransaction(unfunded, None), timeout(5.seconds), interval(500.millis)) { case (tx,fee,changepos) =>
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

  it must "be able to import a p2sh script successfully using importaddress" in {
    val (redeemScript,_) = ScriptGenerators.p2pkhScriptPubKey.sample.get
    val response = test.importAddress(Left(redeemScript))
    whenReady(response, timeout(5.seconds), interval(500.millis)) { r =>

    }
  }


  it must "be able to import a p2sh address successfully using importaddress" in {
    val (redeemScript,_) = ScriptGenerators.p2pkhScriptPubKey.sample.get
    val scriptPubKey = P2SHScriptPubKey(redeemScript)
    val addr = P2SHAddress(scriptPubKey,TestUtil.network)
    val response = test.importAddress(Right(addr))
    whenReady(response, timeout(5.seconds), interval(500.millis)) { r =>

    }
  }

  it must "be able to import a p2sh script successfully using importmulti" in {
    val (redeemScript,privKeys) = ScriptGenerators.smallMultiSigScriptPubKey.sample.get
    val pubKeys = privKeys.map(_.publicKey)
    val scriptPubKey = P2SHScriptPubKey(redeemScript)
    val request = ImportMultiRequest(Left(scriptPubKey),Some(0L),Some(redeemScript),pubKeys,Nil,
      true,true,instance.network)
    val response = test.importMulti(request)
    whenReady(response, timeout(5.seconds), interval(500.millis)) { r =>
      r.success must be (true)
    }
  }

  it must "be able to import a p2sh address successfully using importmulti" in {
    val (redeemScript,privKeys) = ScriptGenerators.smallMultiSigScriptPubKey.sample.get
    val pubKeys = privKeys.map(_.publicKey)
    val scriptPubKey = P2SHScriptPubKey(redeemScript)
    val addr = P2SHAddress(scriptPubKey,TestUtil.network)
    val request = ImportMultiRequest(Right(addr),None,Some(redeemScript),pubKeys,
      privKeys,true,false, instance.network)
    val response = test.importMulti(request)
    whenReady(response, timeout(5.seconds), interval(500.millis)) { r =>
      r.success must be (true)
    }
  }

  it must "send a raw transaction to the network" in {
    val signed = generatedTx
    val sent = signed.flatMap(s => test.sendRawTransaction(s))
    val getrawtx = sent.flatMap { _ =>
      signed.flatMap(s => test.getRawTransaction(s.txId))
    }
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
    val getTx = sent.flatMap { _ =>
      signed.flatMap(s => test.getTransaction(s.txId))
    }
    val allInfo: Future[(Transaction,WalletTransaction)] = getTx.flatMap { gTx =>
      signed.map(s => (s,gTx))
    }
    whenReady(allInfo, timeout(5.seconds), interval(500.millis)) { info =>
      info._1 must be (info._2.transaction)
    }
  }

  it must "get the number of confirmations on a transaction" in {
    val signed = generatedTx
    val sent = signed.flatMap(s => test.sendRawTransaction(s))
    val getConfsZero = sent.flatMap{ txid =>
      val flippedEndianess = DoubleSha256Digest(BitcoinSUtil.flipEndianness(txid.hex))
      test.getConfirmations(flippedEndianess)
    }
    val generated = sent.flatMap(_ => test.generate(10))
    val getConfs10 = generated.flatMap { _ =>
      signed.flatMap { tx =>
        test.getConfirmations(tx.txId)
      }
    }

    //test what happens when a tx is not broadcast at all
    val notBroadcast = generatedTx
    val notBroadcastConfs = notBroadcast.flatMap(tx => test.getConfirmations(tx.txId))
    val successful = getConfsZero.flatMap(z => getConfs10.map(t => (z,t)))
    val confs = successful.flatMap(s => notBroadcastConfs.failed.map(t => (Seq(s._1,s._2),t)))
    whenReady(confs, timeout(5.seconds), interval(500.millis)) { c =>
      c._1(0) must be (Some(0))
      c._1(1) must be (Some(10))
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
    val opts = Some(FundRawTransactionOptions(None,None,Some(true),None,None,None,Nil))
    val funded: Future[(Transaction, CurrencyUnit, Int)] = generateBlocks.flatMap(_ => test.fundRawTransaction(tx,opts))
    val signed: Future[(Transaction,Boolean)] = funded.flatMap(f => test.signRawTransaction(f._1))
    signed.map(_._1)
  }
  override def afterAll = {
    Await.result(TestUtil.stopNodes(Seq(test,test1)), 5.seconds)
    materializer.shutdown()
  }
}
