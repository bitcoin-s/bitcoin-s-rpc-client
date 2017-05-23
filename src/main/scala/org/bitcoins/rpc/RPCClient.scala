package org.bitcoins.rpc

import akka.http.scaladsl.model.{HttpEntity, Uri}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.bitcoins.core.config.{MainNet, RegTest}
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey}
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{BitcoinAddress, P2PKHAddress}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}
import org.bitcoins.rpc.bitcoincore.blockchain.{BlockchainInfo, ConfirmedUnspentTransactionOutput, MemPoolInfo}
import org.bitcoins.rpc.bitcoincore.mining.GetMiningInfo
import org.bitcoins.rpc.bitcoincore.networking.{AddedNodeInfo, NetworkInfo, PeerInfo}
import org.bitcoins.rpc.bitcoincore.wallet._
import org.bitcoins.rpc.config.DaemonInstance
import org.bitcoins.rpc.marshallers.RPCMarshallerUtil
import org.bitcoins.rpc.marshallers.blockchain.{BlockchainInfoRPCMarshaller, ConfirmedUnspentTransactionOutputMarshaller, MemPoolInfoMarshaller}
import org.bitcoins.rpc.marshallers.mining.MiningInfoMarshaller
import org.bitcoins.rpc.marshallers.networking.{AddedNodeInfoMarshaller, NetworkRPCMarshaller}
import org.bitcoins.rpc.marshallers.wallet.UTXOMarshaller._
import org.bitcoins.rpc.marshallers.wallet.WalletMarshaller
import spray.json._

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.sys.process._
import scala.util.Try

/**
  * Created by Tom on 1/14/2016.
  */
sealed trait RPCClient extends RPCMarshallerUtil
  with BitcoinSLogger with DefaultJsonProtocol {
  def instance: DaemonInstance
  implicit val materializer: ActorMaterializer
  implicit val dispatcher = materializer.system.dispatcher
  /**
   * Refer to this reference for list of RPCs
   * https://bitcoin.org/en/developer-reference#rpcs */
  def sendCommand(command : String) : Future[JsObject] = {
    val request = RPCHandler.buildRequest(command)
    sendRequest(request)
  }

  def sendCommand(command: String, arg: Int): Future[JsObject] = {
    val request = RPCHandler.buildRequest(command,arg)
    sendRequest(request)
  }

  def sendCommand(command: String, arg: String): Future[JsObject] = {
    val request = RPCHandler.buildRequest(command,arg)
    sendRequest(request)
  }

  def sendCommand(command: String, arg: Boolean): Future[JsObject] = {
    val request = RPCHandler.buildRequest(command,arg)
    sendRequest(request)
  }

  def sendCommand(command: String, arg: JsArray): Future[JsObject] = {
    val request = RPCHandler.buildRequest(command,arg)
    sendRequest(request)
  }

  def sendCommand(command: String, arg1: String, args: JsObject): Future[JsObject] = {
    val request = RPCHandler.buildRequest(command,arg1,args)
    sendRequest(request)
  }

  private def sendRequest(request: JsObject): Future[JsObject] = {
    val result = RPCHandler.sendRequest(instance,request)
    val source: Future[HttpEntity.Strict] = result.flatMap(_.entity.toStrict(5.seconds)(materializer))
    val response = source.map(_.data.decodeString(ByteString.UTF_8).parseJson.asJsObject)
    response.flatMap(r => checkForError(request,r))
  }

  private def checkForError(request: JsObject, response: JsObject): Future[JsObject] = {
    val f = response.fields
    val errOpt = f("error")
    if (errOpt != JsNull) {
      val errorMsg = "bitcoind returned error for request: " + request.toString + " error: " + errOpt.toString
      logger.error(errorMsg)
      Future.failed(new IllegalArgumentException(errorMsg))
    } else Future.successful(response)
  }

  /** Starts the bitcoind instance */
  def start: Unit = {
    val networkArg = parseNetworkArg
    val datadir = instance.authCredentials.datadir
    val cmd = "bitcoind " + networkArg + " -datadir=" + datadir + " -daemon"
    cmd.!!
  }


  /** This will stop the server */
  def stop: Future[Unit] = {
    val response = sendCommand("stop")
    response.map(_ => ())
  }

  /**
    * The addnode RPC attempts to add or remove a node from the addnode list, or to try a connection to a node once.
    * [[https://bitcoin.org/en/developer-reference#addnode]]
    */
  def addNode(uri: Uri): Future[Unit] = {
    val cmd = "addnode"
    val arr = JsArray(JsString(uri.authority.toString), JsString("add"))
    sendCommand(cmd,arr).map { json =>
      ()
    }
  }

  /** Disconnects following node from your node. */
  def disconnectNode(uri: Uri): Future[Unit] = {
    val cmd = "disconnectnode"
    sendCommand(cmd,uri.authority.toString).map { json =>
      ()
    }
  }

  def getAddedNodeInfo: Future[Seq[AddedNodeInfo]] = {
    val cmd = "getaddednodeinfo"
    sendCommand(cmd).map { json =>
      val f = json.fields
      val arr = f("result").asInstanceOf[JsArray]
      arr.elements.map(e => AddedNodeInfoMarshaller.AddedNodeInfoFormatter.read(e))
    }
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
    sendCommand("getbalance").map(json => Bitcoins(json.fields("result").convertTo[Double]))
  }

  /**
   * The hash of the block header from the most recent block on the best block chain,
   * encoded as hex in RPC byte order
   * https://bitcoin.org/en/developer-reference#getbalance
   */
  def getBestBlockHash: Future[DoubleSha256Digest] = {
    sendCommand("getbestblockhash").map(json => DoubleSha256Digest(json.fields("result").convertTo[String]))
  }

  //Wallet stuff
  /**
    * The estimatefee RPC estimates the transaction fee per kilobyte that needs to be paid for a transaction to be
    * included within a certain number of blocks.
    * [[https://bitcoin.org/en/developer-reference#estimatefee]]
    * @param numBlocks - this needs to be between the number 2 and 25
    * @return The estimated fee the transaction should pay in order to be included within the specified number of blocks.
    *         If the node doesn’t have enough information to make an estimate, the value -1 will be returned
    */
  def estimateFee(numBlocks: Int): Future[CurrencyUnit] = {
    val cmd = "estimatefee"
    sendCommand(cmd,numBlocks).map { json =>
      val result = json.fields("result").convertTo[Double]
      if (result == -1) {
        //defaultFee is satoshis/byte, so multiply by 1000 to get satoshis/kb
        Policy.defaultFee * Satoshis(Int64(1000))
      } else {
        Bitcoins(result)
      }
    }
  }

  /** Funds the given transaction with outputs in the bitcoin core wallet
    * [[https://bitcoin.org/en/developer-reference#fundrawtransaction]]
    * */
  def fundRawTransaction(tx: Transaction, opts: Option[FundRawTransactionOptions]): Future[(Transaction, CurrencyUnit, Int)] = {
    val cmd = "fundrawtransaction"
    sendCommand(cmd,tx.hex).map { json =>
      val result = json.fields("result")
      val f = result.asJsObject.fields
      val newTx = Transaction(f("hex").convertTo[String])
      val fee = Bitcoins(f("fee").convertTo[Double])
      val changePosition = f("changepos").convertTo[Int]
      (newTx,fee,changePosition)
    }
  }

  /**
    * The importaddress RPC adds an address or pubkey script to the wallet without the associated private key,
    * allowing you to watch for transactions affecting that address or pubkey script without being able to spend
    * any of its outputs.
    * [[https://bitcoin.org/en/developer-reference#importaddress]]
    */
  def importAddress(scriptOrAddress: Either[ScriptPubKey, BitcoinAddress]): Future[Unit] = {
    val cmd = "importaddress"
    val hex = scriptOrAddress match {
      case Left(script) => BitcoinSUtil.encodeHex(script.asmBytes)
      case Right(addr) => addr.value
    }
    sendCommand(cmd,hex).map { json =>
      ()
    }
  }

  def importMulti(request: ImportMultiRequest): Future[ImportMultiResponse] = {
    import spray.json._
    import org.bitcoins.rpc.marshallers.wallet.ImportMultiRequestMarshaller._
    import org.bitcoins.rpc.marshallers.wallet.ImportMultiResponseMarshaller._
    val json = request.toJson
    val array = JsArray(JsArray(json))
    val cmd = "importmulti"
    sendCommand(cmd,array).map { json =>
      val result = json.fields("result")
      val responses = result.asInstanceOf[JsArray].elements.map { j =>
        ImportMultiResponseFormatter.read(j)
      }
      responses.head
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
      val signedTx = Transaction(f("hex").convertTo[String])
      val isComplete = f("complete").convertTo[Boolean]
      (signedTx,isComplete)
    }
  }

  /**
    * Sends the given raw transaction
    * The sendrawtransaction RPC validates a transaction and broadcasts it to the peer-to-peer network.
    * [[https://bitcoin.org/en/developer-reference#sendrawtransaction]]
    */
  def sendRawTransaction(tx: Transaction): Future[DoubleSha256Digest] = {
    val cmd = "sendrawtransaction"
    sendCommand(cmd,tx.hex).map { json =>
      val result = json.fields("result")
      val txid = DoubleSha256Digest(result.convertTo[String])
      txid
    }
  }
  /** Gets a raw transaction from the network
    * The getrawtransaction RPC gets a hex-encoded serialized transaction or a JSON object describing the transaction.
    * By default, Bitcoin Core only stores complete transaction data for UTXOs and your own transactions,
    * so the RPC may fail on historic transactions unless you use the non-default txindex=1 in your
    * Bitcoin Core startup settings.
    * [[https://bitcoin.org/en/developer-reference#getrawtransaction]]
    * */
  def getRawTransaction(hash: DoubleSha256Digest): Future[Transaction] = {
    val cmd = "getrawtransaction"
    sendCommand(cmd,hash.hex).map { json =>
      val result = json.fields("result")
      val tx = Transaction(result.convertTo[String])
      tx
    }
  }

  /**
    * The gettransaction RPC gets detailed information about an in-wallet transaction.
    * [[https://bitcoin.org/en/developer-reference#gettransaction]]
    */
  def getTransaction(hash: DoubleSha256Digest): Future[WalletTransaction] = {
    import org.bitcoins.rpc.marshallers.wallet.WalletTransactionMarshaller._
    val cmd = "gettransaction"
    sendCommand(cmd,hash.hex).map { json =>
      val result = json.fields("result")
      val walletTx = result.convertTo[WalletTransaction]
      walletTx
    }
  }

  def listUnspent: Future[Seq[UTXO]] = {
    val cmd = "listunspent"
    sendCommand(cmd).map { json =>
      val utxos: Seq[UTXO] = json.fields("result").convertTo[Seq[UTXO]]
      utxos
    }
  }

  /** Gets the confirmations for a transaction on the network.
    * Note the daemon instance must be started with -txindex for this to work for
    * arbitrary transactions on the network
    */
  def getConfirmations(hash: DoubleSha256Digest): Future[Option[Long]] = {
    val cmd = "getrawtransaction"
    val flipped = BitcoinSUtil.flipEndianness(hash.hex)
    val args = JsArray(JsString(flipped), JsBoolean(true))
    sendCommand(cmd,args).map { json =>
      val result = json.fields("result")
      val confs = Try(result.asJsObject.fields("confirmations").convertTo[Long])
      Some(confs.getOrElse(0L))
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

}

object RPCClient {
  private case class RPCClientImpl(instance: DaemonInstance, materializer: ActorMaterializer) extends RPCClient

  def apply(instance: DaemonInstance, materializer: ActorMaterializer): RPCClient = {
    RPCClientImpl(instance, materializer)
  }

}