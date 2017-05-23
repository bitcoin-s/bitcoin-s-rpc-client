package org.bitcoins.rpc.channels

import org.bitcoins.core.channels._
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.P2SHAddress
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.RPCClient
import org.bitcoins.rpc.bitcoincore.wallet.ImportMultiRequest

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by chris on 5/10/17.
  */
sealed trait PaymentChannelClient {

  def client: RPCClient

  def channel: PaymentChannel

  def clientKey: ECPrivateKey

  def update(amount: CurrencyUnit, clientKey: ECPrivateKey): Future[(PaymentChannelClient, Transaction)] = channel match {
    case _: PaymentChannelAwaitingAnchorTx =>
      Future.failed(new IllegalArgumentException("Cannot sign a payment channel awaiting the anchor transaction, need to provide clientSPK and serverSPK"))
    case inProgress: PaymentChannelInProgress =>
      //update payment channel with confs on anchor tx
      val clientSigned = inProgress.clientSign(amount,clientKey)
      val newClient = clientSigned.map(c => (PaymentChannelClient(client,c,clientKey),c.current.transaction))
      Future.fromTry(newClient)
    case _: PaymentChannelClosed =>
      Future.failed(new IllegalArgumentException("Cannot update a payment channel that is already closed"))
  }

  /** Creates the first spending transaction in the payment channel, then signs it with the client's key */
  def update(clientSPK: ScriptPubKey, amount: CurrencyUnit)(implicit ec: ExecutionContext): Future[(PaymentChannelClient,Transaction)] = {
    require(channel.isInstanceOf[PaymentChannelAwaitingAnchorTx],
      "Cannot create the first spending transaction for a payment channel if the type is NOT PaymentChannelAwaitingAnchorTx")
    val ch = channel.asInstanceOf[PaymentChannelAwaitingAnchorTx]
    //get the amount of confs on the anchor tx
    val updatedConfs = client.getConfirmations(ch.anchorTx.tx.txId)
    val newAwaiting = updatedConfs.flatMap(confs =>
      Future.fromTry(PaymentChannelAwaitingAnchorTx(ch.anchorTx,ch.lock,confs.getOrElse(ch.confirmations))))
    val clientSigned = newAwaiting.flatMap(ch =>
      Future.fromTry(ch.clientSign(clientSPK,amount,clientKey)))
    val newClient = clientSigned.map(c => (PaymentChannelClient(client,c,clientKey),c.current.transaction))
    newClient
  }
}

object PaymentChannelClient extends BitcoinSLogger {
  private case class PaymentChannelClientImpl(client: RPCClient, channel: PaymentChannel,
                                              clientKey: ECPrivateKey) extends PaymentChannelClient

  /** Creates a [[org.bitcoins.core.channels.PaymentChannelAwaitingAnchorTx]],
    * it also broadcasts the [[org.bitcoins.core.channels.AnchorTransaction]]
    * to the network
    */
  def apply(client: RPCClient, serverPublicKey: ECPublicKey, timeout: LockTimeScriptPubKey,
            depositAmount: CurrencyUnit)(implicit ec: ExecutionContext): Future[PaymentChannelClient] = {
    val clientPrivKey = ECPrivateKey()
    val importPrivKey = client.importPrivateKey(clientPrivKey)
    val clientPubKey = clientPrivKey.publicKey
    val escrow = MultiSignatureScriptPubKey(2,Seq(clientPubKey, serverPublicKey))
    val lock = EscrowTimeoutScriptPubKey(escrow,timeout)
    val p2sh = P2SHScriptPubKey(lock)
    val addr = P2SHAddress(p2sh,client.instance.network)
    val importRequest = importPrivKey.map { _ =>
      ImportMultiRequest(Right(addr),None,Some(lock),Seq(serverPublicKey,clientPubKey), Nil,
        false,true, client.instance.network)
    }
    val importMulti = importRequest.flatMap(i => client.importMulti(i))
    val outputs = Seq(TransactionOutput(depositAmount,p2sh))
    val bTx = BaseTransaction(TransactionConstants.validLockVersion,Nil,outputs,
      TransactionConstants.lockTime)
    //fund, sign then broadcast the tx
    val funded: Future[(Transaction,CurrencyUnit,Int)] = importMulti.flatMap { i =>
      client.fundRawTransaction(bTx,None)
    }
    val signed: Future[(Transaction,Boolean)] = funded.flatMap(f => client.signRawTransaction(f._1))
    val sent = signed.flatMap(s => client.sendRawTransaction(s._1))
    val anchorTx = sent.flatMap { _ =>
      signed.map { s =>
        AnchorTransaction(s._1)
      }
    }
    val paymentChannel = anchorTx.flatMap(aTx => Future.fromTry(PaymentChannelAwaitingAnchorTx(aTx,lock)))
    paymentChannel.map(chan => PaymentChannelClient(client,chan,clientPrivKey))
  }

  def apply(client: RPCClient, channel: PaymentChannel, clientKey: ECPrivateKey): PaymentChannelClient = {
    PaymentChannelClientImpl(client,channel, clientKey)
  }
}
