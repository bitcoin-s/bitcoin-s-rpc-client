package org.bitcoins.rpc


import org.bitcoins.protocol.{BitcoinAddress, Address}
import org.bitcoins.rpc.marshallers.blockchain.{ConfirmedUnspentTransactionOutputMarshaller, MemPoolInfoMarshaller, BlockchainInfoRPCMarshaller}
import org.bitcoins.rpc.marshallers.mining.MiningInfoMarshaller
import org.bitcoins.rpc.marshallers.networking.{PeerInfoRPCMarshaller, NetworkRPCMarshaller}
import org.bitcoins.rpc.marshallers.wallet.WalletMarshaller
import org.scalatest.{MustMatchers, FlatSpec}
import org.bitcoins.rpc.marshallers.RPCMarshallerUtil
import spray.json._

import scala.sys.process.Process

/**
  * Created by tom on 4/26/16.
  */
class ScalaRPCClientTest extends FlatSpec with MustMatchers {
  val client : String = "bitcoin-cli"
  val network : String = "-regtest"
  //val network : String = "-testnet"
  val test = new ScalaRPCClient(client, network)
  //bitcoind -rpcuser=$RPC_USER -rpcpassword=$RPC_PASS -regtest -txindex -daemon

  "ScalaRPCClient" must "send a command to the command line and return the output" in {
    test.sendCommand("generate 101")
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

  it must "get block hash" in {
    test.getBlock(0) must be ("0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206")
  }

  it must "add a 1-of-1 multisig address" in {
    test.sendCommand("importprivkey cRWEGSNfu7HB8V5doyDaRkWtEUe3jmpSminuD5F9Jyq3f9xxst2t")
    val address = "n3Dj9Utyu9EXxux4En49aHs59PdYStvang"
    test.generateOneOfOneMultiSigAddress(address) must be (BitcoinAddress("2MtuY5ef3sGdBfdJUDyYUTYGPJU7Ef14vhB"))
  }

  it must "stop the server" in {
    test.stopServer
  }

}
