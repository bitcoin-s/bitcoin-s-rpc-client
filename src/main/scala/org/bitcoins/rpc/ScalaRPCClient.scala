package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest}
import org.bitcoins.rpc.marshallers.RPCMarshallerUtil
import org.bitcoins.rpc.marshallers.blockchain.{BlockchainInfoRPCMarshaller, ConfirmedUnspentTransactionOutputMarshaller, MemPoolInfoMarshaller}
import org.bitcoins.rpc.marshallers.mining.MiningInfoMarshaller
import org.bitcoins.rpc.marshallers.networking.NetworkRPCMarshaller
import org.bitcoins.rpc.marshallers.wallet.WalletMarshaller
import org.bitcoins.rpc.bitcoincore.blockchain.{BlockchainInfo, ConfirmedUnspentTransactionOutput, MemPoolInfo}
import org.bitcoins.rpc.bitcoincore.mining.GetMiningInfo
import org.bitcoins.rpc.bitcoincore.networking.{NetworkInfo, PeerInfo}
import org.bitcoins.rpc.bitcoincore.wallet.WalletInfo
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.util.BitcoinSLogger
import spray.json._

import scala.concurrent.Future
import scala.sys.process._

/**
  * Created by Tom on 1/14/2016.
  */
class ScalaRPCClient(network : NetworkParameters, username: String, password: String, system: ActorSystem,
                     datadir: String = System.getProperty("user.home") + "/.bitcoin") extends RPCMarshallerUtil with BitcoinSLogger {


  /**
   * Refer to this reference for list of RPCs
   * https://bitcoin.org/en/developer-reference#rpcs */
  def sendCommand(command : String) : String = {
    val networkArg = if (network == MainNet) "" else "-" + network.name
    val cmd = "bitcoin-cli " + networkArg + " -datadir=" + datadir + " " + command
    val result = cmd.!!
    result
  }

  /** Starts the bitcoind instance */
  def start: String = {
    val networkArg = if (network == MainNet) "" else "-" + network.name
    val cmd = "bitcoind " + networkArg + " -datadir=" + datadir + " -daemon"
    val result = cmd.!!
    result
  }


  /** This will stop the server */
  def stop: Future[Unit] = {
    val json = RPCHandler.buildRequest("stop")
    val uri = Uri("http://localhost:" + network.rpcPort)
    val response = RPCHandler.sendRequest(username,password,uri,json)
    response.map(_ => ())(system.dispatcher)
  }

  /**
   * The number of blocks in the local best block chain. For a new node with only the hardcoded genesis block,
   * this number will be 0
   * https://bitcoin.org/en/developer-reference#getblockcount
   */
  def getBlockCount : Int = sendCommand("getblockcount").trim.toInt

  /** Generates an arbitrary number of blocks in regtest */
  def generate(num: Int): String = {
    require(network == RegTest, "Can only generate blocks in regtest")
    sendCommand("generate " + num)
  }
  /**
   * The getmempoolinfo RPC returns information about the node's current transaction memory pool.
   * https://bitcoin.org/en/developer-reference#getmempoolinfo
   */
  def getMemPoolInfo : MemPoolInfo = {
    val result : String = sendCommand("getmempoolinfo")
    MemPoolInfoMarshaller.MemPoolInfoFormatter.read(result.parseJson)
  }

  /**
   * Information about the current state of the local block chain.
   * https://bitcoin.org/en/developer-reference#getblockchaininfo
   */
  def getBlockChainInfo : BlockchainInfo = {
    val result : String = sendCommand("getblockchaininfo")
    BlockchainInfoRPCMarshaller.BlockchainInfoFormatter.read(result.parseJson)
  }

  /**
   * The gettxoutsetinfo RPC returns statistics about the confirmed unspent transaction output (UTXO) set.
   * Note that this call may take some time and that it only counts outputs from confirmed transactions—it does
   * not count outputs from the memory pool.
   * https://bitcoin.org/en/developer-reference#gettxoutsetinfo
   */
  def getTxOutSetInfo : ConfirmedUnspentTransactionOutput = {
    val result : String = sendCommand("gettxoutsetinfo")
    ConfirmedUnspentTransactionOutputMarshaller.ConfirmedUnspentTransactionOutputFormatter.read(result.parseJson)
  }

  /**
   * The getmininginfo RPC returns various mining-related information.
   * https://bitcoin.org/en/developer-reference#getmininginfo
   */
  def getMiningInfo : GetMiningInfo = {
    val result : String = sendCommand("getmininginfo")
    MiningInfoMarshaller.MiningInfoFormatter.read(result.parseJson)
  }

  /**
   * The getnetworkinfo RPC returns information about the node’s connection to the network.
   * https://bitcoin.org/en/developer-reference#getnetworkinfo
   */
  def getNetworkInfo : NetworkInfo = {
    val result : String = sendCommand("getnetworkinfo")
    NetworkRPCMarshaller.NetworkInfoFormatter.read(result.parseJson)
  }

  /**
   * The getpeerinfo RPC returns data about each connected network node.
   * https://bitcoin.org/en/developer-reference#getpeerinfo
   */
  def getPeerInfo : Seq[PeerInfo] = {
    val result : String = sendCommand("getpeerinfo")
    val json = result.parseJson
    convertToPeerInfoSeq(json)
  }

  /**
   * The getwalletinfo RPC provides information about the wallet.
   * https://bitcoin.org/en/developer-reference#getwalletinfo
   */
  def getWalletInfo : WalletInfo = {
    val result : String = sendCommand("getwalletinfo")
    WalletMarshaller.WalletFormatter.read(result.parseJson)
  }

  /**
   * The difficulty of creating a block with the same target threshold (nBits) as the highest-height block in the local
   * best block chain. The number is a a multiple of the minimum difficulty
   * https://bitcoin.org/en/developer-reference#getdifficulty
   */
  def getDifficulty : Double = sendCommand("getdifficulty").trim.toDouble

  /**
   * The getnewaddress RPC returns a new Bitcoin address for receiving payments. If an account is specified,
   * payments received with the address will be credited to that account.
   * https://bitcoin.org/en/developer-reference#getnewaddress
   */
  def getNewAddress: BitcoinAddress = BitcoinAddress(sendCommand("getnewaddress").trim)

  /**
   * The getrawchangeaddress RPC returns a new Bitcoin address for receiving change. This is for use with raw
   * transactions, not normal use.
   * https://bitcoin.org/en/developer-reference#getrawchangeaddress */
  def getRawChangeAddress: BitcoinAddress = BitcoinAddress(sendCommand("getrawchangeaddress").trim)

  /**
   * The getbalance RPC gets the balance in decimal bitcoins across all accounts or for a particular account.
   * https://bitcoin.org/en/developer-reference#getbalance */
  def getBalance : Double = sendCommand("getbalance").toDouble

  /**
   * The hash of the block header from the most recent block on the best block chain,
   * encoded as hex in RPC byte order
   * https://bitcoin.org/en/developer-reference#getbalance
   */
  def getBestBlockHash : String = sendCommand("getbestblockhash")

  /**
    * Given an address, returns a 1-of-1 p2sh address and adds it to the wallet.
    * https://bitcoin.org/en/developer-reference#addmultisigaddress
    */
  def generateOneOfOneMultiSigAddress(sigsRequired : Int, address: String): BitcoinAddress = {
    //val cmd = "addmultisigaddress " + sigsRequired + " [\\\"" + address + "\\\"]"
    val addressInJson = "[\"" + address + "\"]"
    val cmd = "addmultisigaddress 1 " + " " + addressInJson
    val result : String = sendCommand(cmd)
    BitcoinAddress(result.trim)
  }

}

