package org.bitcoins.rpc.channels

import org.bitcoins.core.channels._
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, WitnessTxSigComponent}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.P2SHAddress
import org.bitcoins.core.protocol.script.{EscrowTimeoutScriptPubKey, ScriptPubKey}
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.RPCClient
import org.bitcoins.rpc.bitcoincore.wallet.ImportMultiRequest

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
  /** The [[ECPrivateKey]] the server uses to sign the [[Channel]] with */
  def serverKey: ECPrivateKey

  /** The last instance of a payment channel that the client has signed
    * This is useful for closing the [[Channel]]
    */
  def lastClientSigned: Option[ChannelInProgressClientSigned]

  /** Updates our [[Channel]] with a new partially signed [[WitnessTransaction]] from the client,
    * This function signs the witness transaction with the servers private key and returns a new
    * [[ChannelServer]] instance with the new [[ChannelInProgress]]
    *
    */
  def update(partiallySignedTx: WitnessTransaction, changeClientSPK: ScriptPubKey)(implicit ec: ExecutionContext): Future[ChannelServer] = channel match {
    case awaiting: ChannelAwaitingAnchorTx =>
      val confs = client.getConfirmations(awaiting.anchorTx.txId)
      val newAwaiting = confs.flatMap { c =>
        Future.fromTry(ChannelAwaitingAnchorTx(awaiting.anchorTx, awaiting.lock, c.getOrElse(awaiting.confirmations)))
      }
      val clientSigned = newAwaiting.map(a => a.createClientSigned(partiallySignedTx,changeClientSPK))
      val fullySigned: Future[ChannelInProgress] = clientSigned.flatMap {
        case Some(c) => Future.fromTry(c.serverSign(serverKey))
        case None => Future.failed(new IllegalArgumentException("ClientSPK not founded on the partiallySignedTx"))
      }
      val newServer = fullySigned.flatMap { f =>
        clientSigned.flatMap { c =>
          ChannelServer(client,serverKey,f,c)
        }
      }
      newServer
    case inProgress: ChannelInProgress =>
      val current = inProgress.current
      val updated = WitnessTxSigComponent(partiallySignedTx,current.inputIndex,inProgress.scriptPubKey,
        current.flags,inProgress.lockedAmount)
      val clientSigned = ChannelInProgressClientSigned(inProgress.anchorTx,inProgress.lock,
        inProgress.clientChangeSPK, updated, current +: inProgress.old)
      val fullySigned = Future.fromTry(clientSigned.serverSign(serverKey))
      val newServer = fullySigned.flatMap { f =>
        ChannelServer(client,serverKey,f,Some(clientSigned))
      }
      newServer
    case _: ChannelInProgressClientSigned =>
      Future.failed(new IllegalArgumentException("Cannot update a payment channel to client signed if the channel is " +
        "already signed by the client awaiting a server's signature"))
    case _: ChannelClosed =>
      Future.failed(new IllegalArgumentException("Cannot update a payment channel when the payment channel is closed"))
  }

  /** Closes the payment channel, returns the final transaction */
  def close(serverSPK: ScriptPubKey)(implicit ex: ExecutionContext): Future[WitnessTransaction] = channel match {
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
                    serverKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[WitnessTransaction] = {
    val feeEstimate = client.estimateFee(Policy.confirmations.toInt)
    val txSize = clientSigned.current.transaction.bytes.size
    val fee: Future[CurrencyUnit] = feeEstimate.map { f =>
      val fullFee = f * Satoshis(Int64(txSize))
      logger.info("Fee for closing tx: " + fullFee)
      fullFee
    }

    val closed: Future[ChannelClosed] = fee.flatMap { f =>
      Future.fromTry(clientSigned.close(serverSPK, serverKey, f))
    }

    val broadcast = closed.flatMap { c =>
      logger.info("Broadcasting tx for closed payment channel: " + c.current.transaction.hex)
      val txid = client.sendRawTransaction(c.current.transaction)
      txid.map(t => logger.info("Broadcast txid: " + t))
      txid
    }
    broadcast.flatMap { _ =>
      closed.map(_.current.transaction)
    }
  }
}

object ChannelServer extends BitcoinSLogger {
  private case class ChannelServerImpl(client: RPCClient, channel: Channel, serverKey: ECPrivateKey,
                                              lastClientSigned: Option[ChannelInProgressClientSigned]) extends ChannelServer

  def apply(client: RPCClient, serverKey: ECPrivateKey, txId: DoubleSha256Digest,
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
      awaiting.flatMap(a => ChannelServer(client,serverKey,a,None))
    }
  }

  def apply(client: RPCClient, serverKey: ECPrivateKey, channel: Channel,
            lastClientSigned: Option[ChannelInProgressClientSigned])(implicit ec: ExecutionContext): Future[ChannelServer] = {
    val importPrivKey = client.importPrivateKey(serverKey)
    importPrivKey.map(_ => ChannelServerImpl(client,channel, serverKey: ECPrivateKey, lastClientSigned))
  }
}
