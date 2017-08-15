package org.bitcoins.rpc.channels

import org.bitcoins.core.channels._
import org.bitcoins.core.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{P2PKHAddress, P2SHAddress}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.RPCClient
import org.bitcoins.rpc.bitcoincore.wallet.ImportMultiRequest

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by chris on 5/10/17.
  */
sealed trait ChannelClient extends BitcoinSLogger {

  def client: RPCClient

  def channel: Channel

  def clientKey: ECPrivateKey

  def update(amount: CurrencyUnit): Future[(ChannelClient, Transaction)] = channel match {
    case _: ChannelAwaitingAnchorTx =>
      Future.failed(new IllegalArgumentException("Cannot sign a payment channel awaiting the anchor transaction, need to provide clientSPK and serverSPK"))
    case inProgress: ChannelInProgress =>
      val clientSigned = inProgress.clientSign(amount,clientKey)
      //TODO: This is hacky, but we want the current value to be of type ChannelInProgress, NOT ChannelInProgressClientSigned
      val ip = clientSigned.map(c => ChannelInProgress(inProgress.anchorTx,inProgress.lock,inProgress.clientChangeSPK,c.current,c.old))
      val newClient = ip.map(c => (ChannelClient(client,c,clientKey),c.current.transaction))
      Future.fromTry(newClient)
    case _: ChannelInProgressClientSigned =>
      //TODO: Look at this case, is this right?
      Future.failed(new IllegalArgumentException("Channel was ChannelInProgressClientSigned"))
    case _: ChannelClosed =>
      Future.failed(new IllegalArgumentException("Cannot update a payment channel that is already closed"))
  }

  /** Creates the first spending transaction in the payment channel, then signs it with the client's key */
  def update(clientSPK: ScriptPubKey, amount: CurrencyUnit)(implicit ec: ExecutionContext): Future[(ChannelClient,WitnessTransaction)] = {
    val invariant = Future(require(channel.isInstanceOf[ChannelAwaitingAnchorTx],
      "Cannot create the first spending transaction for a payment channel if the type is NOT ChannelAwaitingAnchorTx"))
    val ch = invariant.map(_ => channel.asInstanceOf[ChannelAwaitingAnchorTx])
    //get the amount of confs on the anchor tx
    val updatedConfs = ch.flatMap(c => client.getConfirmations(c.anchorTx.txId))
    val newAwaiting = ch.flatMap { c =>
      updatedConfs.flatMap(confs =>
        Future.fromTry(ChannelAwaitingAnchorTx(c.anchorTx, c.lock, confs.getOrElse(c.confirmations))))
    }
    val clientSigned = newAwaiting.flatMap(ch =>
      Future.fromTry(ch.clientSign(clientSPK,amount,clientKey)))
    //TODO: This is hacky, but we want the current value to be of type ChannelInProgress, NOT ChannelInProgressClientSigned
    val ip = clientSigned.map(c => ChannelInProgress(c.anchorTx,c.lock,c.clientChangeSPK,c.current,c.old))
    val newClient = ip.map(c => (ChannelClient(client,c,clientKey),c.current.transaction))
    newClient
  }

  def closeWithTimeout(implicit ec: ExecutionContext): Future[Transaction] = {
    val invariant = Future(require(channel.lock.timeout.nestedScriptPubKey.isInstanceOf[P2PKHScriptPubKey]))
    val nestedTimeoutSPK = invariant.map(_ => channel.lock.timeout.nestedScriptPubKey.asInstanceOf[P2PKHScriptPubKey])
    val timeoutKey = nestedTimeoutSPK.flatMap(spk => client.dumpPrivateKey(P2PKHAddress(spk,client.instance.network)))
    val closedWithTimeout = channel match {
      case awaiting: ChannelAwaitingAnchorTx =>
        val address = client.getNewAddress
        //we don't have anything to estimate a fee based upon, so create the tx first with
        //one satoshis as the fee
        val txSigComponentNoFee = timeoutKey.flatMap { key =>
          address.flatMap { addr =>
            Future.fromTry(awaiting.closeWithTimeout(addr.scriptPubKey, key, Satoshis.one))
          }
        }
        val closed = timeoutKey.flatMap { key =>
          txSigComponentNoFee.flatMap { t =>
            address.flatMap { addr =>
              //re-estimate the fee now that we have an idea what the size of the tx actually is
              val fee = estimateFeeForTx(t.current.transaction)
              fee.flatMap{ f =>
                Future.fromTry(awaiting.closeWithTimeout(addr.scriptPubKey, key,f)) }
            }
          }
        }
        closed
      case inProgress: ChannelInProgress =>
        val estimatedFee = estimateFeeForTx(inProgress.current.transaction)
        val closed = timeoutKey.flatMap { key =>
          estimatedFee.flatMap(f => Future.fromTry(inProgress.closeWithTimeout(key, f)))
        }
        closed
      case inProgress: ChannelInProgressClientSigned =>
        val estimatedFee = estimateFeeForTx(inProgress.current.transaction)
        val closed = timeoutKey.flatMap { key =>
          estimatedFee.flatMap(f => Future.fromTry(inProgress.closeWithTimeout(key, f)))
        }
        closed
      case _: ChannelClosed =>
        Future.failed(new IllegalArgumentException("Cannot close a payment channel that has already been closed"))
    }

    val sendRawTx = closedWithTimeout.flatMap(c => client.sendRawTransaction(c.current.transaction))
    sendRawTx.flatMap { txid =>
      logger.info("Closed with timeout txid: " + txid)
      closedWithTimeout.map(_.current.transaction)
    }
  }


  private def estimateFeeForTx(tx: Transaction)(implicit ec: ExecutionContext): Future[CurrencyUnit] = {
    val estimateFeePerByte = client.estimateFee(Policy.confirmations.toInt)
    estimateFeePerByte.map(f => f * Satoshis(Int64(tx.bytes.size)))
  }
}

object ChannelClient extends BitcoinSLogger {
  private case class ChannelClientImpl(client: RPCClient, channel: Channel,
                                              clientKey: ECPrivateKey) extends ChannelClient

  /** Creates a [[org.bitcoins.core.channels.ChannelAwaitingAnchorTx]],
    * it also broadcasts the anchor tx to the network
    */
  def apply(client: RPCClient, serverPublicKey: ECPublicKey, timeout: LockTimeScriptPubKey,
            depositAmount: CurrencyUnit)(implicit ec: ExecutionContext): Future[ChannelClient] = {
    require(timeout.nestedScriptPubKey.isInstanceOf[P2PKHScriptPubKey], "We only support P2PKHScriptPubKey's for timeout branches for a payment channel currently")
    val clientPrivKey = ECPrivateKey()
    val importPrivKey = client.importPrivateKey(clientPrivKey)
    val clientPubKey = clientPrivKey.publicKey
    val escrow = MultiSignatureScriptPubKey(2,Seq(clientPubKey, serverPublicKey))
    val lock = EscrowTimeoutScriptPubKey(escrow,timeout)
    val witSPK = WitnessScriptPubKeyV0(lock)
    val p2sh = P2SHScriptPubKey(witSPK)
    val addr = P2SHAddress(p2sh,client.instance.network)
    val importMulti = importPrivKey.map { _ =>
      val i = ImportMultiRequest(Right(addr),None,Some(lock),Seq(serverPublicKey,clientPubKey), Nil,
        false,true, client.instance.network)
      client.importMulti(i)
    }
    val outputs = Seq(TransactionOutput(depositAmount,p2sh))
    val bTx = BaseTransaction(TransactionConstants.validLockVersion,Nil,outputs,
      TransactionConstants.lockTime)
    //fund, sign then broadcast the tx
    val funded: Future[(Transaction,CurrencyUnit,Int)] = importMulti.flatMap { i =>
      client.fundRawTransaction(bTx,None)
    }
    val signed: Future[(Transaction,Boolean)] = funded.flatMap { f =>
      client.signRawTransaction(f._1)
    }
    val sent = signed.flatMap { s =>
      client.sendRawTransaction(s._1)
    }
    val anchorTx = sent.flatMap { _ =>
      val tx = signed.map { t =>
        logger.info("Anchor txid: " + t._1.txId)
        logger.info("Anchor transaction: " + t._1.hex)
        t._1
      }
      tx
    }
    val channel = anchorTx.flatMap(aTx => Future.fromTry(ChannelAwaitingAnchorTx(aTx,lock)))
    channel.map(chan => ChannelClient(client,chan,clientPrivKey))
  }

  def apply(client: RPCClient, channel: Channel, clientKey: ECPrivateKey): ChannelClient = {
    ChannelClientImpl(client,channel, clientKey)
  }
}
