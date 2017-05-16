package org.bitcoins.rpc.channels

import org.bitcoins.core.channels._
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, WitnessTxSigComponent}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{EscrowTimeoutScriptPubKey, ScriptPubKey}
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.RPCClient

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

  /** Updates our [[PaymentChannel]] with a new partially signed [[WitnessTransaction]] from the client,
    * This function signs the witness transaction with the servers private key and returns a new
    * [[PaymentChannelServer]] instance with the new [[PaymentChannelInProgress]]
    */
  def update(partiallySignedWTx: WitnessTransaction, serverScriptPubKey: ScriptPubKey, amount: CurrencyUnit,
             privKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[PaymentChannelServer] = channel match {
    case awaiting: PaymentChannelAwaitingAnchorTx =>
      val confs: Future[Int] = client.getConfirmations(awaiting.anchorTx.tx.txId)
      val newAwaiting = confs.flatMap { c =>
        Future.fromTry(PaymentChannelAwaitingAnchorTx(awaiting.anchorTx, awaiting.lock, c))
      }
      val clientSigned = newAwaiting.map(a => a.createClientSigned(partiallySignedWTx,UInt32.zero,serverScriptPubKey,
        amount,Policy.standardScriptVerifyFlags))
      val fullySigned = clientSigned.flatMap(c => Future.fromTry(c.serverSign(privKey, HashType.sigHashSingle)))
      val newServer = fullySigned.map(c => PaymentChannelServer(client,c))
      newServer
    case inProgress: PaymentChannelInProgress =>
      val current = inProgress.current
      val updated = WitnessTxSigComponent(partiallySignedWTx,current.inputIndex,inProgress.scriptPubKey,
        current.flags,current.amount)
      val clientSigned = PaymentChannelInProgressClientSigned(inProgress.anchorTx,inProgress.lock,updated,current +: inProgress.old,serverScriptPubKey)
      val fullySigned = Future.fromTry(clientSigned.serverSign(privKey, HashType.sigHashSingle))
      val newServer = fullySigned.map(c => PaymentChannelServer(client,c))
      newServer
    case _: PaymentChannelClosed =>
      Future.failed(new IllegalArgumentException("Cannot update a payment channel when the payment channel is closed"))
  }

  /** Closes the payment channel, returns the final transaction */
  def close(implicit ex: ExecutionContext): Future[Transaction] = channel match {
    case _: PaymentChannelAwaitingAnchorTx =>
      Future.failed(new IllegalArgumentException("Cannot close a payment channel that is awaiting anchor tx"))
    case inProgress: PaymentChannelInProgress =>
      val closed = inProgress.close
      val finalTx = closed.finalTx.transaction
      //adds a fee to the transaction
      client.getBalance.map(c => logger.error("c: " + c))
      val funded = client.fundRawTransaction(finalTx)
      val broadcast = funded.flatMap(f => client.sendRawTransaction(f._1))
      broadcast.flatMap { _ =>
        funded.map(_._1)
      }
    case _: PaymentChannelClosed =>
      Future.failed(new IllegalArgumentException("Cannot close a payment channel that is already closed"))
  }
}

object PaymentChannelServer {
  private case class PaymentChannelServerImpl(client: RPCClient, channel: PaymentChannel) extends PaymentChannelServer

  def apply(client: RPCClient, txId: DoubleSha256Digest,
            lock: EscrowTimeoutScriptPubKey)(implicit ec: ExecutionContext): Future[PaymentChannelServer] = {
    val transaction = client.getRawTransaction(txId)
    val anchorTransaction = transaction.map(AnchorTransaction(_))
    val awaiting = anchorTransaction.flatMap(aTx => Future.fromTry(PaymentChannelAwaitingAnchorTx(aTx,lock)))
    awaiting.map(a => PaymentChannelServer(client,a))
  }

  def apply(client: RPCClient, channel: PaymentChannel): PaymentChannelServer = {
    PaymentChannelServerImpl(client,channel)
  }
}
