package org.bitcoins.rpc

import java.io.PrintWriter

import akka.actor.ActorSystem
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.protocol.{Address, BitcoinAddress}
import org.bitcoins.rpc.marshallers.blockchain.{BlockchainInfoRPCMarshaller, ConfirmedUnspentTransactionOutputMarshaller, MemPoolInfoMarshaller}
import org.bitcoins.rpc.marshallers.mining.MiningInfoMarshaller
import org.bitcoins.rpc.marshallers.networking.{NetworkRPCMarshaller, PeerInfoRPCMarshaller}
import org.bitcoins.rpc.marshallers.wallet.WalletMarshaller
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionOutput}
import org.scalatest.{BeforeAndAfterAll, FlatSpec, MustMatchers}
import org.bitcoins.rpc.marshallers.RPCMarshallerUtil
import spray.json._

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.sys.process.Process

/**
  * Created by tom on 4/26/16.
  */
class ScalaRPCClientTest extends FlatSpec with MustMatchers with BeforeAndAfterAll {
  val system = ActorSystem("ScalaRPCClientTest")
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

  val test = new ScalaRPCClient(network,username,password,system,datadir)
  //bitcoind -rpcuser=$RPC_USER -rpcpassword=$RPC_PASS -regtest -txindex -daemon

  override def beforeAll: Unit = {
    test.start
    Thread.sleep(20000)
  }
  "ScalaRPCClient" must "send a command to the command line and return the output" in {
    test.generate(101)
    test.getBlockCount must be(101)
  }

  it must "parse and return networkinfo" in {
    val networkInfo = test.sendCommand("getnetworkinfo")
    val json = networkInfo.parseJson
    test.getNetworkInfo must be (NetworkRPCMarshaller.NetworkInfoFormatter.read(json))
    test.getNetworkInfo.version must be (NetworkRPCMarshaller.NetworkInfoFormatter.read(json).version)
  }

  it must "parse and return mininginfo" in {
    val miningInfo = test.sendCommand("getmininginfo")
    val json = miningInfo.parseJson
    test.getMiningInfo must be (MiningInfoMarshaller.MiningInfoFormatter.read(json))
  }

  it must "parse and return blockchaininfo" in {
    val blockchainInfo = test.sendCommand("getblockchaininfo")
    val json = blockchainInfo.parseJson
    test.getBlockChainInfo must be (BlockchainInfoRPCMarshaller.BlockchainInfoFormatter.read(json))
    test.getBlockChainInfo.chain must be ("regtest")
  }

  it must "parse and return peer info" in {
    val peerInfo = test.sendCommand("getpeerinfo")
    val json = peerInfo.parseJson
    test.getPeerInfo must be (test.convertToPeerInfoSeq(json))
  }

  it must "parse and return mempoolinfo" in {
    val mempoolInfo = test.sendCommand("getmempoolinfo")
    val json = mempoolInfo.parseJson
    test.getMemPoolInfo must be (MemPoolInfoMarshaller.MemPoolInfoFormatter.read(json))
  }

  it must "parse and return txoutset info" in {
    val txOutSetInfo = test.sendCommand("gettxoutsetinfo")
    val json = txOutSetInfo.parseJson
    test.getTxOutSetInfo must be {
      ConfirmedUnspentTransactionOutputMarshaller.ConfirmedUnspentTransactionOutputFormatter.read(json)
    }
  }

  it must "parse and return wallet info" in {
    val walletInfo = test.sendCommand("getwalletinfo")
    val json = walletInfo.parseJson
    test.getWalletInfo must be (WalletMarshaller.WalletFormatter.read(json))
  }

  it must "get difficuluty" in {
    test.getDifficulty must be (4.656542373906925E-10)
  }

  it must "get new address" in {
    test.getNewAddress.isInstanceOf[BitcoinAddress] must be (true)
  }

  it must "get raw change address" in {
    test.getRawChangeAddress.isInstanceOf[BitcoinAddress] must be (true)
  }

  it must "get the balance" in {
    test.getBalance must be (50.0)
  }

  it must "list utxos" in {
    test.listUnspent.nonEmpty must be (true)
  }

/*  it must "fund a raw transaction" in {
    val output = TransactionOutput(CurrencyUnits.oneBTC,EmptyScriptPubKey)
    val unfunded = Transaction(TransactionConstants.version,Nil,Seq(output),TransactionConstants.lockTime)
    val (tx,fees,changepos) = test.fundRawTransaction(unfunded)


  }*/

  override def afterAll = {
    system.terminate()
    Await.result(test.stop,5.seconds)
  }
}
