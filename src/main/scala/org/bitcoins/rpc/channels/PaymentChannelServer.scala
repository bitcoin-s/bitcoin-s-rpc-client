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
sealed trait PaymentChannelServer extends BitcoinSLogger {

  /** RPC client used to interact with bitcoin network */
  def client: RPCClient

  /** The [[org.bitcoins.core.channels.PaymentChannel]] shared between
    * the [[PaymentChannelClient]] and [[PaymentChannelServer]] */
  def channel: PaymentChannel

  /** The last instance of a payment channel that the client has signed
    * This is useful for closing the [[PaymentChannel]]
    */
  def lastClientSigned: Option[PaymentChannelInProgressClientSigned]

  /** Updates our [[PaymentChannel]] with a new partially signed [[WitnessTransaction]] from the client,
    * This function signs the witness transaction with the servers private key and returns a new
    * [[PaymentChannelServer]] instance with the new [[PaymentChannelInProgress]]
    */
  def update(partiallySignedWTx: Transaction,
             privKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[PaymentChannelServer] = channel match {
    case awaiting: PaymentChannelAwaitingAnchorTx =>
      val confs = client.getConfirmations(awaiting.anchorTx.tx.txId)
      val newAwaiting = confs.flatMap { c =>
        Future.fromTry(PaymentChannelAwaitingAnchorTx(awaiting.anchorTx, awaiting.lock, c.getOrElse(awaiting.confirmations)))
      }
      val clientSigned = newAwaiting.map(a => a.createClientSigned(partiallySignedWTx,UInt32.zero,
        Policy.standardScriptVerifyFlags))
      val fullySigned = clientSigned.flatMap(c => Future.fromTry(c.serverSign(privKey)))
      val newServer = fullySigned.flatMap { f =>
        clientSigned.map { c =>
          PaymentChannelServer(client,f,Some(c))
        }
      }
      newServer
    case inProgress: PaymentChannelInProgress =>
      val current = inProgress.current
      val updated = TxSigComponent(partiallySignedWTx,current.inputIndex,inProgress.scriptPubKey,
        current.flags)
      val clientSigned = PaymentChannelInProgressClientSigned(inProgress.anchorTx,inProgress.lock,
        updated,current +: inProgress.old)
      val fullySigned = Future.fromTry(clientSigned.serverSign(privKey))
      val newServer = fullySigned.map { f =>
        PaymentChannelServer(client,f,Some(clientSigned))
      }
      newServer
    case _: PaymentChannelClosed =>
      Future.failed(new IllegalArgumentException("Cannot update a payment channel when the payment channel is closed"))
  }

  /** Closes the payment channel, returns the final transaction */
  def close(serverSPK: ScriptPubKey, serverKey: ECPrivateKey)(implicit ex: ExecutionContext): Future[Transaction] = channel match {
    case _: PaymentChannelAwaitingAnchorTx =>
      Future.failed(new IllegalArgumentException("Cannot close a payment channel that is awaiting anchor tx"))
    case clientSigned: PaymentChannelInProgressClientSigned =>
      close(clientSigned,serverSPK,serverKey)
    case _: PaymentChannelInProgress =>
      val result = lastClientSigned.map(c => close(c,serverSPK,serverKey))
      result match {
        case Some(tx) => tx
        case None => Future.failed(
          new IllegalArgumentException("Cannot close a payment channel that has never been client signed"))
      }
    case _: PaymentChannelClosed =>
      Future.failed(new IllegalArgumentException("Cannot close a payment channel that is already closed"))
  }
  /** Helper function to close a payment channel */
  private def close(clientSigned: PaymentChannelInProgressClientSigned, serverSPK: ScriptPubKey,
                    serverKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[Transaction] = {
    val feeEstimatePerKB = client.estimateFee(Policy.confirmations.toInt)
    //convert fee estimate to satoshis
    //TODO: Review this, the .underlying.underlying is ugly
    val feePerByte = feeEstimatePerKB.map(c => c.satoshis.underlying.underlying / 1000)
    val txSize = clientSigned.current.transaction.bytes.size
    val fee: Future[CurrencyUnit] = feePerByte.map(f => Satoshis(Int64(txSize * f)))

    val closed: Future[PaymentChannelClosed] = fee.flatMap { f =>
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

object PaymentChannelServer extends BitcoinSLogger {
  private case class PaymentChannelServerImpl(client: RPCClient, channel: PaymentChannel,
                                              lastClientSigned: Option[PaymentChannelInProgressClientSigned]) extends PaymentChannelServer

  def apply(client: RPCClient, txId: DoubleSha256Digest,
            lock: EscrowTimeoutScriptPubKey)(implicit ec: ExecutionContext): Future[PaymentChannelServer] = {
    val transaction = client.getRawTransaction(txId)
    val anchorTransaction = transaction.map(AnchorTransaction(_))
    val awaiting = anchorTransaction.flatMap(aTx => Future.fromTry(PaymentChannelAwaitingAnchorTx(aTx,lock)))

    val importMulti = awaiting.flatMap { a =>
      val addr = P2SHAddress(a.scriptPubKey, client.instance.network)
      val request = ImportMultiRequest(Right(addr),None,Some(a.lock),Nil,
        Nil,false,true,client.instance.network)
      client.importMulti(request)
    }
    importMulti.flatMap { _ =>
      awaiting.map(a => PaymentChannelServer(client,a,None))
    }
  }

  def apply(client: RPCClient, channel: PaymentChannel,
            lastClientSigned: Option[PaymentChannelInProgressClientSigned]): PaymentChannelServer = {
    PaymentChannelServerImpl(client,channel, lastClientSigned)
  }
}
