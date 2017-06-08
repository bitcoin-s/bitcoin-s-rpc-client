package org.bitcoins.rpc.channels

import org.bitcoins.core.channels._
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, TxSigComponent, WitnessTxSigComponent}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.P2SHAddress
import org.bitcoins.core.protocol.script.{EscrowTimeoutScriptPubKey, ScriptPubKey}
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.RPCClient
import org.bitcoins.rpc.bitcoincore.wallet.{FundRawTransactionOptions, ImportMultiRequest}
import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by chris on 5/10/17.
  */
sealed trait ChannelServer extends BitcoinSLogger {

  /** RPC client used to interact with bitcoin network */
  def client: RPCClient

  /** The [[org.bitcoins.core.channels.Channel]] shared between
    * the [[ChannelClient]] and [[ChannelServer]] */
  def channel: Channel

  /** The last instance of a payment channel that the client has signed
    * This is useful for closing the [[Channel]]
    */
  def lastClientSigned: Option[ChannelInProgressClientSigned]

  /** Updates our [[Channel]] with a new partially signed [[WitnessTransaction]] from the client,
    * This function signs the witness transaction with the servers private key and returns a new
    * [[ChannelServer]] instance with the new [[ChannelInProgress]]
    */
  def update(partiallySignedTx: Transaction, clientSPK: ScriptPubKey,
             privKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[ChannelServer] = channel match {
    case awaiting: ChannelAwaitingAnchorTx =>
      val confs = client.getConfirmations(awaiting.anchorTx.txId)
      val newAwaiting = confs.flatMap { c =>
        Future.fromTry(ChannelAwaitingAnchorTx(awaiting.anchorTx, awaiting.lock, c.getOrElse(awaiting.confirmations)))
      }
      val clientSigned = newAwaiting.map(a => a.createClientSigned(partiallySignedTx,clientSPK))
      val fullySigned: Future[ChannelInProgress] = clientSigned.flatMap {
        case Some(c) => Future.fromTry(c.serverSign(privKey))
        case None => Future.failed(new IllegalArgumentException("ClientSPK not founded on the partiallySignedTx"))
      }
      val newServer = fullySigned.flatMap { f =>
        clientSigned.map { c =>
          ChannelServer(client,f,c)
        }
      }
      newServer
    case inProgress: ChannelInProgress =>
      val current = inProgress.current
      val updated = TxSigComponent(partiallySignedTx,current.inputIndex,inProgress.scriptPubKey,
        current.flags)
      val clientSigned = ChannelInProgressClientSigned(inProgress.anchorTx,inProgress.lock,
        inProgress.clientSPK, updated, current +: inProgress.old)
      val fullySigned = Future.fromTry(clientSigned.serverSign(privKey))
      val newServer = fullySigned.map { f =>
        ChannelServer(client,f,Some(clientSigned))
      }
      newServer
    case _: ChannelClosed =>
      Future.failed(new IllegalArgumentException("Cannot update a payment channel when the payment channel is closed"))
  }

  /** Closes the payment channel, returns the final transaction */
  def close(serverSPK: ScriptPubKey, serverKey: ECPrivateKey)(implicit ex: ExecutionContext): Future[Transaction] = channel match {
    case _: ChannelAwaitingAnchorTx =>
      Future.failed(new IllegalArgumentException("Cannot close a payment channel that is awaiting anchor tx"))
    case clientSigned: ChannelInProgressClientSigned =>
      close(clientSigned,serverSPK,serverKey)
    case _: ChannelInProgress =>
      val result = lastClientSigned.map(c => close(c,serverSPK,serverKey))
      result match {
        case Some(tx) => tx
        case None => Future.failed(
          new IllegalArgumentException("Cannot close a payment channel that has never been client signed"))
      }
    case _: ChannelClosed =>
      Future.failed(new IllegalArgumentException("Cannot close a payment channel that is already closed"))
  }

  /** Helper function to close a payment channel */
  private def close(clientSigned: ChannelInProgressClientSigned, serverSPK: ScriptPubKey,
                    serverKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[Transaction] = {
    val feeEstimatePerKB = client.estimateFee(Policy.confirmations.toInt)
    //convert fee estimate to satoshis
    //TODO: Review this, the .underlying.underlying is ugly
    val feePerByte = feeEstimatePerKB.map(c => c.satoshis.underlying.underlying / 1000)
    val txSize = clientSigned.partiallySigned.transaction.bytes.size
    val fee: Future[CurrencyUnit] = feePerByte.map(f => Satoshis(Int64(txSize * f)))
    fee.map(f => logger.info("Fee for closing tx: " + f))
    val closed: Future[ChannelClosed] = fee.flatMap { f =>
      Future.fromTry(clientSigned.close(serverSPK, serverKey, f))
    }

    val broadcast = closed.flatMap { c =>
      logger.info("Broadcasting tx for closed payment channel: " + c.finalTx.transaction.hex)
      val txid = client.sendRawTransaction(c.finalTx.transaction)
      txid.map(t => logger.info("Broadcast txid: " + t))
      txid
    }
    broadcast.flatMap { _ =>
      closed.map(_.finalTx.transaction)
    }
  }
}

object ChannelServer extends BitcoinSLogger {
  private case class ChannelServerImpl(client: RPCClient, channel: Channel,
                                              lastClientSigned: Option[ChannelInProgressClientSigned]) extends ChannelServer

  def apply(client: RPCClient, txId: DoubleSha256Digest,
            lock: EscrowTimeoutScriptPubKey)(implicit ec: ExecutionContext): Future[ChannelServer] = {
    val transaction = client.getRawTransaction(txId)
    val awaiting = transaction.flatMap(aTx => Future.fromTry(ChannelAwaitingAnchorTx(aTx,lock)))

    val importMulti = awaiting.flatMap { a =>
      val addr = P2SHAddress(a.scriptPubKey, client.instance.network)
      val request = ImportMultiRequest(Right(addr),None,Some(a.lock),Nil,
        Nil,false,true,client.instance.network)
      client.importMulti(request)
    }
    importMulti.flatMap { _ =>
      awaiting.map(a => ChannelServer(client,a,None))
    }
  }

  def apply(client: RPCClient, channel: Channel,
            lastClientSigned: Option[ChannelInProgressClientSigned]): ChannelServer = {
    ChannelServerImpl(client,channel, lastClientSigned)
  }
}
