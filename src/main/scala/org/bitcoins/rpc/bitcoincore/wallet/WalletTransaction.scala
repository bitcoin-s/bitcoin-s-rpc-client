package org.bitcoins.rpc.bitcoincore.wallet

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.transaction.Transaction

/**
  * Created by chris on 5/4/17.
  * Meant to represent a wallet tranaction returned by this RPC call
  * [[https://bitcoin.org/en/developer-reference#gettransaction]]
  */
sealed trait WalletTransaction {
  /** A positive number of bitcoins if this transaction increased the total wallet balance;
    * a negative number of bitcoins
    * if this transaction decreased the total wallet balance,
    * or 0 if the transaction had no net effect on wallet balance
    * */
  def amount: CurrencyUnit
  /** If an outgoing transaction, this is the fee paid by the transaction reported as negative bitcoins */
  def fee: CurrencyUnit
  def confirmations: Int

  /** Set to true if the transaction is a coinbase. Not returned for regular transactions */
  def generated: Option[Boolean]
  def blockHash: Option[DoubleSha256Digest]
  def blockIndex: Option[Long]
  def blockTime: Option[Long]
  def txId: DoubleSha256Digest

  /** An array containing the TXIDs of other transactions that spend
    * the same inputs (UTXOs) as this transaction. Array may be empty
    * */
  def walletConflicts: Seq[DoubleSha256Digest]
  /** A Unix epoch time when the transaction was added to the wallet */
  def time: Long
  /** A Unix epoch time when the transaction was detected by the local node,
    * or the time of the block on the local best block chain that included the transaction */
  def timeReceived: Long
  /** Indicates if a transaction is replaceable under BIP 125:
    • yes is replaceable
    • no not replaceable
    • unknown for unconfirmed transactions not in the mempool */
  def bip125Replaceable: String
  def comment: Option[String]
  def to: Option[String]
  def transaction: Transaction
}


object WalletTransaction {
  private case class WalletTransactionImpl(amount: CurrencyUnit, fee: CurrencyUnit, confirmations: Int, generated: Option[Boolean],
                                          blockHash: Option[DoubleSha256Digest], blockIndex: Option[Long], blockTime: Option[Long],
                                          txId: DoubleSha256Digest, walletConflicts: Seq[DoubleSha256Digest], time: Long,
                                          timeReceived: Long, bip125Replaceable: String, comment: Option[String], to: Option[String],
                                          transaction: Transaction) extends WalletTransaction

  def apply(amount: CurrencyUnit, fee: CurrencyUnit, confirmations: Int, generated: Option[Boolean],
            blockHash: Option[DoubleSha256Digest], blockIndex: Option[Long], blockTime: Option[Long],
            txId: DoubleSha256Digest, walletConflicts: Seq[DoubleSha256Digest], time: Long,
            timeReceived: Long, bip125Replaceable: String, comment: Option[String], to: Option[String],
            transaction: Transaction): WalletTransaction = {
    WalletTransactionImpl(amount,fee,confirmations,generated,blockHash, blockIndex, blockTime, txId,
      walletConflicts,time,timeReceived,bip125Replaceable,comment,to,transaction)
  }
}