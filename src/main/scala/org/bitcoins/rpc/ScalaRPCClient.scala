package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpEntity, Uri}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest}
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.rpc.marshallers.RPCMarshallerUtil
import org.bitcoins.rpc.marshallers.blockchain.{BlockchainInfoRPCMarshaller, ConfirmedUnspentTransactionOutputMarshaller, MemPoolInfoMarshaller}
import org.bitcoins.rpc.marshallers.mining.MiningInfoMarshaller
import org.bitcoins.rpc.marshallers.networking.NetworkRPCMarshaller
import org.bitcoins.rpc.marshallers.wallet.WalletMarshaller
import org.bitcoins.rpc.bitcoincore.blockchain.{BlockchainInfo, ConfirmedUnspentTransactionOutput, MemPoolInfo}
import org.bitcoins.rpc.bitcoincore.mining.GetMiningInfo
import org.bitcoins.rpc.bitcoincore.networking.{NetworkInfo, PeerInfo}
import org.bitcoins.rpc.bitcoincore.wallet.{UTXO, WalletInfo}
import org.bitcoins.core.protocol.{BitcoinAddress, P2PKHAddress}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.config.BitcoindInstance
import spray.json._
import org.bitcoins.rpc.marshallers.wallet.UTXOMarshaller._

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.sys.process._

/**
  * Created by Tom on 1/14/2016.
  */
class ScalaRPCClient(instance: BitcoindInstance, username: String, password: String, materializer: ActorMaterializer,
                     datadir: String = System.getProperty("user.home") + "/.bitcoin") extends RPCMarshallerUtil
  with BitcoinSLogger with DefaultJsonProtocol {

  implicit val dispatcher = materializer.system.dispatcher
  /**
   * Refer to this reference for list of RPCs
   * https://bitcoin.org/en/developer-reference#rpcs */
  def sendCommand(command : String) : Future[JsObject] = {
    val request = RPCHandler.buildRequest(command)
    val result = RPCHandler.sendRequest(username,password,instance,request)
    val source: Future[HttpEntity.Strict] = result.flatMap(_.entity.toStrict(5.seconds)(materializer))
    source.map { ent =>
      val decoded = ent.data.decodeString(ByteString.UTF_8)
      decoded.parseJson.asJsObject
    }
  }

  def sendCommand(command: String, arg: Int): Future[JsObject] = {
    val request = RPCHandler.buildRequest(command,arg)
    val result = RPCHandler.sendRequest(username,password,instance,request)
    val source: Future[HttpEntity.Strict] = result.flatMap(_.entity.toStrict(5.seconds)(materializer))
    source.map(_.data.decodeString(ByteString.UTF_8).parseJson.asJsObject)
  }

  def sendCommand(command: String, arg: String): Future[JsObject] = {
    val request = RPCHandler.buildRequest(command,arg)
    val result = RPCHandler.sendRequest(username,password,instance,request)
    val source: Future[HttpEntity.Strict] = result.flatMap(_.entity.toStrict(5.seconds)(materializer))
    source.map(_.data.decodeString(ByteString.UTF_8).parseJson.asJsObject)
  }

  def sendCommand(command: String, arg: JsArray): Future[JsObject] = {
    val request = RPCHandler.buildRequest(command,arg)
    val result = RPCHandler.sendRequest(username,password,instance,request)
    val source: Future[HttpEntity.Strict] = result.flatMap(_.entity.toStrict(5.seconds)(materializer))
    source.map(_.data.decodeString(ByteString.UTF_8).parseJson.asJsObject)
  }

  /** Starts the bitcoind instance */
  def start: Unit = {
    val networkArg = parseNetworkArg
    val cmd = "bitcoind " + networkArg + " -datadir=" + datadir + " -daemon"
    cmd.!!
  }


  /** This will stop the server */
  def stop: Future[Unit] = {
    val response = sendCommand("stop")
    response.map(_ => ())
  }

  /**
   * The number of blocks in the local best block chain. For a new node with only the hardcoded genesis block,
   * this number will be 0
   * https://bitcoin.org/en/developer-reference#getblockcount
   */
  def getBlockCount : Future[Int] = {
    sendCommand("getblockcount").map(json => json.fields("result").convertTo[Int])
  }

  /** Generates an arbitrary number of blocks in regtest */
  def generate(num: Int): Future[Seq[DoubleSha256Digest]] = {
    require(instance.network == RegTest, "Can only generate blocks in regtest")
    val result = sendCommand("generate",num)
    result.map { r =>
      r.fields("result") match {
        case arr: JsArray =>
          arr.elements.map(hash => DoubleSha256Digest(hash.toString()))
        case x @ _ => throw new IllegalArgumentException("Expected JsArray from bitcoin-cli generate, got: " + x)
      }
    }
  }
  /**
   * The getmempoolinfo RPC returns information about the node's current transaction memory pool.
   * https://bitcoin.org/en/developer-reference#getmempoolinfo
   */
  def getMemPoolInfo: Future[MemPoolInfo] = {
    val result = sendCommand("getmempoolinfo")
    result.map(r => MemPoolInfoMarshaller.MemPoolInfoFormatter.read(r))
  }

  /**
   * Information about the current state of the local block chain.
   * https://bitcoin.org/en/developer-reference#getblockchaininfo
   */
  def getBlockChainInfo: Future[BlockchainInfo] = {
    val result = sendCommand("getblockchaininfo")
    result.map(r => BlockchainInfoRPCMarshaller.BlockchainInfoFormatter.read(r))
  }

  /**
   * The gettxoutsetinfo RPC returns statistics about the confirmed unspent transaction output (UTXO) set.
   * Note that this call may take some time and that it only counts outputs from confirmed transactions—it does
   * not count outputs from the memory pool.
   * https://bitcoin.org/en/developer-reference#gettxoutsetinfo
   */
  def getTxOutSetInfo: Future[ConfirmedUnspentTransactionOutput] = {
    val result = sendCommand("gettxoutsetinfo")
    result.map(r =>
      ConfirmedUnspentTransactionOutputMarshaller.ConfirmedUnspentTransactionOutputFormatter.read(r))
  }

  /**
   * The getmininginfo RPC returns various mining-related information.
   * https://bitcoin.org/en/developer-reference#getmininginfo
   */
  def getMiningInfo: Future[GetMiningInfo] = {
    val result = sendCommand("getmininginfo")
    result.map(r => MiningInfoMarshaller.MiningInfoFormatter.read(r))
  }

  /**
   * The getnetworkinfo RPC returns information about the node’s connection to the network.
   * https://bitcoin.org/en/developer-reference#getnetworkinfo
   */
  def getNetworkInfo: Future[NetworkInfo] = {
    val result = sendCommand("getnetworkinfo")
    result.map(r => NetworkRPCMarshaller.NetworkInfoFormatter.read(r))
  }

  /**
   * The getpeerinfo RPC returns data about each connected network node.
   * https://bitcoin.org/en/developer-reference#getpeerinfo
   */
  def getPeerInfo : Future[Seq[PeerInfo]] = {
    val result = sendCommand("getpeerinfo")
    result.map(r => convertToPeerInfoSeq(r))
  }

  /**
   * The getwalletinfo RPC provides information about the wallet.
   * https://bitcoin.org/en/developer-reference#getwalletinfo
   */
  def getWalletInfo: Future[WalletInfo] = {
    val result = sendCommand("getwalletinfo")
    result.map(r => WalletMarshaller.WalletFormatter.read(r))
  }

  /**
   * The difficulty of creating a block with the same target threshold (nBits) as the highest-height block in the local
   * best block chain. The number is a a multiple of the minimum difficulty
   * https://bitcoin.org/en/developer-reference#getdifficulty
   */
  def getDifficulty: Future[Double] = {
    sendCommand("getdifficulty").map(json => json.fields("result").convertTo[Double])
  }

  /**
   * The getnewaddress RPC returns a new Bitcoin address for receiving payments. If an account is specified,
   * payments received with the address will be credited to that account.
   * https://bitcoin.org/en/developer-reference#getnewaddress
   */
  def getNewAddress: Future[BitcoinAddress] = {
    sendCommand("getnewaddress").map(json => BitcoinAddress(json.fields("result").convertTo[String]))
  }

  /**
   * The getrawchangeaddress RPC returns a new Bitcoin address for receiving change. This is for use with raw
   * transactions, not normal use.
   * https://bitcoin.org/en/developer-reference#getrawchangeaddress */
  def getRawChangeAddress: Future[BitcoinAddress] = {
    sendCommand("getrawchangeaddress").map(json => BitcoinAddress(json.fields("result").convertTo[String]))
  }

  /**
   * The getbalance RPC gets the balance in decimal bitcoins across all accounts or for a particular account.
   * https://bitcoin.org/en/developer-reference#getbalance */
  def getBalance: Future[CurrencyUnit] = {
    sendCommand("getbalance").map(json => doubleToCurrencyUnit(json.fields("result").convertTo[Double]))
  }

  /**
   * The hash of the block header from the most recent block on the best block chain,
   * encoded as hex in RPC byte order
   * https://bitcoin.org/en/developer-reference#getbalance
   */
  def getBestBlockHash: Future[DoubleSha256Digest] = {
    sendCommand("getbestblockhash").map(json => DoubleSha256Digest(json.fields("result").convertTo[String]))
  }

  /** Funds the given transaction with outputs in the bitcoin core wallet
    * [[https://bitcoin.org/en/developer-reference#fundrawtransaction]]
    * */
  def fundRawTransaction(tx: Transaction): Future[(Transaction, CurrencyUnit, Int)] = {
    val cmd = "fundrawtransaction"
    sendCommand(cmd,tx.hex).map { json =>
      val result = json.fields("result")
      val f = result.asJsObject.fields
      val newTx = Transaction(f("hex").convertTo[String])
      val sat = f("fee").convertTo[Double]
      val fee = doubleToCurrencyUnit(sat)
      val changePosition = f("changepos").convertTo[Int]
      (newTx,fee,changePosition)
    }
  }

  /** Signs the given raw transaction
    * [[https://bitcoin.org/en/developer-reference#signrawtransaction]]
    * */
  def signRawTransaction(tx: Transaction): Future[(Transaction, Boolean)] = {
    val cmd = "signrawtransaction"
    sendCommand(cmd,tx.hex).map { json =>
      val result = json.fields("result")
      val f = result.asJsObject.fields
      val signedTx = Transaction(f("result").convertTo[String])
      val isComplete = f("complete").convertTo[Boolean]
      (signedTx,isComplete)
    }
  }

  def listUnspent: Future[Seq[UTXO]] = {
    val cmd = "listunspent"
    sendCommand(cmd).map { json =>
      val utxos: Seq[UTXO] = json.fields("result").convertTo[Seq[UTXO]]
      utxos
    }
  }

  /**
    * Imports the private key into bitcoin core's wallet
    * [[https://bitcoin.org/en/developer-reference#importprivkey]]
    */
  def importPrivateKey(privKey: ECPrivateKey): Future[Boolean] = {
    val key = privKey.toWIF(instance.network)
    sendCommand("importprivkey",key).map(json => json.fields.keys.isEmpty)
  }

  /**
    * Dumps the private key associated with this address
    * [[https://bitcoin.org/en/developer-reference#dumpprivkey]]
    */
  def dumpPrivateKey(address: P2PKHAddress): Future[ECPrivateKey] = {
    sendCommand("dumpprivkey",address.value).map { json =>
      ECPrivateKey.fromWIFToPrivateKey(json.fields("result").convertTo[String])
    }
  }

  private def parseNetworkArg: String = {
    if (instance.network == MainNet) "" else "-" + instance.network.name
  }

  private def doubleToCurrencyUnit(d: Double): CurrencyUnit = {
    val sat = d * CurrencyUnits.btcToSatoshiScalar
    require(sat.toLong == sat)
    Satoshis(Int64(sat.toLong))
  }
}

