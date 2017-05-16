package org.bitcoins.rpc.channels

import org.bitcoins.core.channels._
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.RPCClient

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by chris on 5/10/17.
  */
sealed trait PaymentChannelClient {

  def client: RPCClient

  def channel: PaymentChannel

  def update(amount: CurrencyUnit, clientKey: ECPrivateKey): Future[(PaymentChannelClient, WitnessTransaction)] = channel match {
    case _: PaymentChannelAwaitingAnchorTx =>
      Future.failed(new IllegalArgumentException("Cannot sign a payment channel awaiting the anchor transaction, need to provide clientSPK and serverSPK"))
    case inProgress: PaymentChannelInProgress =>
      //update payment channel with confs on anchor tx
      val clientSigned = inProgress.clientSign(amount,clientKey,HashType.sigHashSingle)
      val newClient = clientSigned.map(c => (PaymentChannelClient(client,c),c.current.transaction))
      Future.fromTry(newClient)
    case _: PaymentChannelClosed =>
      Future.failed(new IllegalArgumentException("Cannot update a payment channel that is already closed"))
  }

  /** Creates the first spending transaction in the payment channel, then signs it with the client's key */
  def update(clientSPK: ScriptPubKey, serverSPK: ScriptPubKey, amount: CurrencyUnit,
             clientKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[(PaymentChannelClient,WitnessTransaction)] = {
    require(channel.isInstanceOf[PaymentChannelAwaitingAnchorTx],
      "Cannot create the first spending transaction for a payment channel if the type is NOT PaymentChannelAwaitingAnchorTx")
    val ch = channel.asInstanceOf[PaymentChannelAwaitingAnchorTx]
    //get the amount of confs on the anchor tx
    val updatedConfs = client.getConfirmations(ch.anchorTx.tx.txId)
    val newAwaiting = updatedConfs.flatMap(confs =>
      Future.fromTry(PaymentChannelAwaitingAnchorTx(ch.anchorTx,ch.lock,confs)))
    val clientSigned = newAwaiting.flatMap(ch =>
      Future.fromTry(ch.clientSign(clientSPK,serverSPK,amount,clientKey,HashType.sigHashSingle)))
    val newClient = clientSigned.map(c => (PaymentChannelClient(client,c),c.current.transaction))
    newClient
  }
}

object PaymentChannelClient extends BitcoinSLogger {
  private case class PaymentChannelClientImpl(client: RPCClient, channel: PaymentChannel) extends PaymentChannelClient

  /** Creates a [[org.bitcoins.core.channels.PaymentChannelAwaitingAnchorTx]],
    * it also broadcasts the [[org.bitcoins.core.channels.AnchorTransaction]]
    * to the network
    */
  def apply(client: RPCClient, clientPubKey: ECPublicKey, serverPublicKey: ECPublicKey, timeout: LockTimeScriptPubKey,
            depositAmount: CurrencyUnit)(implicit ec: ExecutionContext): Future[PaymentChannelClient] = {
    val escrow = MultiSignatureScriptPubKey(2,Seq(clientPubKey, serverPublicKey))
    val lock = EscrowTimeoutScriptPubKey(escrow,timeout)
    val witScriptPubKey = WitnessScriptPubKeyV0(lock)
    val outputs = Seq(TransactionOutput(depositAmount,witScriptPubKey))
    val txWitness = EmptyWitness
    val wTx = WitnessTransaction(TransactionConstants.validLockVersion,Nil,outputs,
      TransactionConstants.lockTime,txWitness)
    //fund, sign then broadcast the tx
    val funded: Future[(Transaction,CurrencyUnit,Int)] = client.fundRawTransaction(wTx)
    val signed: Future[(Transaction,Boolean)] = funded.flatMap(f => client.signRawTransaction(f._1))
    val sent = signed.flatMap(s => client.sendRawTransaction(s._1))
    val anchorTx = sent.flatMap { _ =>
      signed.map { s =>
        AnchorTransaction(s._1)
      }
    }
    val paymentChannel = anchorTx.flatMap(aTx => Future.fromTry(PaymentChannelAwaitingAnchorTx(aTx,lock)))
    paymentChannel.map(chan => PaymentChannelClient(client,chan))
  }

  def apply(client: RPCClient, channel: PaymentChannel): PaymentChannelClient = {
    PaymentChannelClientImpl(client,channel)
  }
}
