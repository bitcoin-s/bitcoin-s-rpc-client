package org.bitcoins.rpc


import org.bitcoins.protocol.{BitcoinAddress, Address}
import org.bitcoins.rpc.marshallers.blockchain.{ConfirmedUnspentTransactionOutputMarshaller, MemPoolInfoMarshaller, BlockchainInfoRPCMarshaller}
import org.bitcoins.rpc.marshallers.mining.MiningInfoMarshaller
import org.bitcoins.rpc.marshallers.networking.NetworkRPCMarshaller
import org.bitcoins.rpc.marshallers.wallet.WalletMarshaller
import org.scalatest.{MustMatchers, FlatSpec}
import spray.json._

import scala.sys.process.Process

/**
  * Created by tom on 4/26/16.
  */
class ScalaRPCClientTest extends FlatSpec with MustMatchers {
  val client : String = "bitcoin-cli"
  val network : String = "-regtest"
  val test = new ScalaRPCClient(client, network)
  //bitcoind -rpcuser=$RPC_USER -rpcpassword=$RPC_PASS -testnet -txindex -daemon

  "ScalaRPCClient" must "send a command to the command line and return the output" in {
    test.getBlockCount must be (101)
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
    test.getDifficulty must be (0.00000000)
  }

  it must "get new address" in {
    val address = test.getNewAddress
    address.isInstanceOf[BitcoinAddress] must be (true)
  }

  it must "get raw change address" in {
    val rawchangeaddress = test.getRawChangeAddress
    rawchangeaddress.isInstanceOf[BitcoinAddress] must be (true)
  }

  it must "get the balance" in {
    test.getBalance must be (49.99999809)
  }

  it must "get best block hash" in {
    test.getBestBlockHash.trim must be ("7152749bcde70b851181cefbd984a56b6b6451d62b60e3d3e55c010ef6def8a1")
  }

  it must "add a 1-of-1 multisig address" in {
    test.sendCommand("importprivkey cRrskGCyaw2WrAtkA1z4DpWiNnpBz5k98kqZhbDRu8KzUyTxi8QZ")
    val address = "mp1z2kUC5pDv2CkqvfVrxw2J9PVeqGeuQJ"
    test.generateOneOfOneMultiSigAddress(1, address) must be (BitcoinAddress("2MwpMdVo1jXffJjyfynD9Zu9nrZBwdLxBzD"))
  }

}
