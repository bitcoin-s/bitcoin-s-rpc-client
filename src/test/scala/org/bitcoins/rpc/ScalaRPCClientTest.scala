package org.bitcoins.rpc

import java.io.PrintWriter

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionOutput}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.config.BitcoindInstance
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpec, MustMatchers}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

/**
  * Created by tom on 4/26/16.
  */
class ScalaRPCClientTest extends FlatSpec with MustMatchers with ScalaFutures with
  BeforeAndAfterAll with BitcoinSLogger {
  implicit val actorSystem = ActorSystem("ScalaRPCClientTest")
  val materializer = ActorMaterializer()
  implicit val dispatcher = materializer.system.dispatcher
  def randomDirName: String = 0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString
  val (datadir,username,password) = {
    val d = "/tmp/" + randomDirName
    val f = new java.io.File(d)
    f.mkdir()
    val conf = new java.io.File(f.getAbsolutePath + "/bitcoin.conf")
    conf.createNewFile()
    val username = "random_user_name"
    val pass = randomDirName
    val pw = new PrintWriter(conf)
    pw.write("rpcuser=" + username + "\n")
    pw.write("rpcpassword=" + pass + "\n")
    pw.close()
    (d,username,pass)
  }
  val network = RegTest
  val instance = BitcoindInstance(network, Uri("http://localhost:" + network.rpcPort))
  val test = new ScalaRPCClient(instance,username,password,materializer,datadir)
  //bitcoind -rpcuser=$RPC_USER -rpcpassword=$RPC_PASS -regtest -txindex -daemon

  override def beforeAll: Unit = {
    test.start
    Thread.sleep(20000)
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
      satoshis must be (Satoshis(Int64(50L * CurrencyUnits.btcToSatoshiScalar)))
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
    val address = P2PKHAddress(key.publicKey,network)
    val imp = test.importPrivateKey(key)
    val dumpedKeyFuture = imp.flatMap(_ => test.dumpPrivateKey(address))
    whenReady(dumpedKeyFuture, timeout(5.seconds), interval(500.millis)) { dumpedKey =>
      dumpedKey must be (key)
    }
  }

  override def afterAll = {
    materializer.shutdown()
    Await.result(test.stop,5.seconds)
  }
}
