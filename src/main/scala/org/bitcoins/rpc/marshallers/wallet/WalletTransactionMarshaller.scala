package org.bitcoins.rpc.marshallers.wallet

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.bitcoincore.wallet.{UTXO, WalletTransaction}
import spray.json.{DefaultJsonProtocol, JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

import scala.util.Try

/**
  * Created by chris on 5/4/17.
  */
object WalletTransactionMarshaller extends DefaultJsonProtocol {



  val amountKey = "amount"
  val feeKey = "fee"
  val confirmationsKey = "confirmations"
  val generatedKey = "generated"
  val blockHashKey = "blockhash"
  val blockIndexKey = "blockindex"
  val blockTimeKey = "blocktime"
  val txIdKey = "txid"
  val walletConflictsKey = "walletconflicts"
  val timeKey = "time"
  val timeReceivedKey = "timereceived"
  val bip125ReplaceableKey = "bip125-replaceable"
  val commentKey = "comment"
  val toKey = "to"
  val hexKey = "hex"

  implicit object WalletTransactionFormatter extends RootJsonFormat[WalletTransaction] {

    override def read(value: JsValue): WalletTransaction = {
      val f = value.asJsObject.fields
      val amount = Bitcoins(f(amountKey).convertTo[Double])
      val fee = Bitcoins(f(feeKey).convertTo[Double])
      val confirmations = f(confirmationsKey).convertTo[Int]
      val generated = Try(Some(f(generatedKey).convertTo[Boolean])).getOrElse(None)
      val blockHashStr: Option[String] = Try(Some(f(blockHashKey).convertTo[String])).getOrElse(None)
      val blockHash: Option[DoubleSha256Digest] = blockHashStr.map(DoubleSha256Digest(_))
      val blockIndex: Option[Long] = Try(Some(f(blockIndexKey).convertTo[Long])).getOrElse(None)
      val blockTime = Try(Some(f(blockTimeKey).convertTo[Long])).getOrElse(None)
      val txId = DoubleSha256Digest(f(txIdKey).convertTo[String])
      val walletConflicts: Seq[DoubleSha256Digest] = f(walletConflictsKey) match {
        case arr: JsArray => arr.elements.map(e => DoubleSha256Digest(e.convertTo[String])).toSeq
        case _ => throw new IllegalArgumentException("Wallet conflicts must be JsArray")
      }
      val time = f(timeKey).convertTo[Long]
      val timeReceived = f(timeReceivedKey).convertTo[Long]
      val bip125Replaceable = f(bip125ReplaceableKey).convertTo[String]
      val comment = Try(Some(f(commentKey).convertTo[String])).getOrElse(None)
      val to = Try(Some(f(toKey).convertTo[String])).getOrElse(None)
      val tx = Transaction(f(hexKey).convertTo[String])
      WalletTransaction(amount,fee,confirmations,generated,blockHash,blockIndex,blockTime,txId,
        walletConflicts,time,timeReceived, bip125Replaceable,comment,to,tx)
    }

    override def write(walletTx: WalletTransaction): JsValue = {
      val m : Map[String,JsValue] = Map(
        amountKey -> JsNumber(Bitcoins(walletTx.amount.satoshis).toBigDecimal),
        feeKey -> JsNumber(Bitcoins(walletTx.amount.satoshis).toBigDecimal),
        confirmationsKey -> JsNumber(walletTx.confirmations),
        generatedKey -> (if (walletTx.generated.isDefined) JsBoolean(walletTx.generated.get) else JsNull),
        blockHashKey -> (if (walletTx.blockHash.isDefined) JsString(walletTx.blockHash.get.hex) else JsNull),
        blockIndexKey -> (if (walletTx.blockIndex.isDefined) JsNumber(walletTx.blockIndex.get) else JsNull),
        blockTimeKey -> (if (walletTx.blockTime.isDefined) JsNumber(walletTx.blockTime.get) else JsNull),
        txIdKey -> JsString(walletTx.txId.hex),
        walletConflictsKey -> JsArray(walletTx.walletConflicts.map(h => JsString(h.hex)).toVector),
        timeKey -> JsNumber(walletTx.time),
        timeReceivedKey -> JsNumber(walletTx.timeReceived),
        bip125ReplaceableKey -> JsString(walletTx.bip125Replaceable),
        commentKey -> (if (walletTx.comment.isDefined) JsString(walletTx.comment.get) else JsNull),
        toKey -> (if (walletTx.to.isDefined) JsString(walletTx.to.get) else JsNull),
        hexKey -> JsString(walletTx.transaction.hex)
      )
      JsObject(m)
    }
  }
}
