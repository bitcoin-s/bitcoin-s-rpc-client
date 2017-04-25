package org.bitcoins.rpc

import org.bitcoins.core.protocol.{Address, BitcoinAddress}
import org.bitcoins.rpc.marshallers.blockchain.{BlockchainInfoRPCMarshaller, ConfirmedUnspentTransactionOutputMarshaller, MemPoolInfoMarshaller}
import org.bitcoins.rpc.marshallers.mining.MiningInfoMarshaller
import org.bitcoins.rpc.marshallers.networking.{NetworkRPCMarshaller, PeerInfoRPCMarshaller}
import org.bitcoins.rpc.marshallers.wallet.WalletMarshaller
import org.bitcoins.core.protocol.BitcoinAddress
import org.scalatest.{BeforeAndAfterAll, FlatSpec, MustMatchers}
import org.bitcoins.rpc.marshallers.RPCMarshallerUtil
import spray.json._

import scala.sys.process.Process

/**
  * Created by tom on 4/26/16.
  */
class ScalaRPCClientTest extends FlatSpec with MustMatchers with BeforeAndAfterAll {
  def randomDirName: String = 0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString
  val datadir: String = {
    val d = "/tmp/" + randomDirName
    val f = new java.io.File(d)
    f.mkdir()
    d
  }
  val client: String = "bitcoin-cli"
  val network: String = "regtest"

  val test = new ScalaRPCClient(client, network, datadir)
  //bitcoind -rpcuser=$RPC_USER -rpcpassword=$RPC_PASS -regtest -txindex -daemon

  override def beforeAll: Unit = {
    test.start
    Thread.sleep(15000)
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

  override def afterAll = {
    test.stop
  }
}
