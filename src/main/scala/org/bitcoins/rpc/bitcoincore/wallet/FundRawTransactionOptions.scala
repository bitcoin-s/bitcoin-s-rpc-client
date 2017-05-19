package org.bitcoins.rpc.bitcoincore.wallet

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress

/**
  * Created by chris on 5/19/17.
  * [[https://bitcoin.org/en/developer-reference#fundrawtransaction]]
  */
sealed trait FundRawTransactionOptions {

  /** The bitcoin address to receive the change. If not set, the address is chosen from address pool */
  def changeAddress: Option[BitcoinAddress]

  /** The index of the change output. If not set, the change position is randomly chosen */
  def changePosition: Option[Int]

  /** Inputs from watch-only addresses are also considered. The default is false */
  def includeWatching: Option[Boolean]
  /** The selected outputs are locked after running the rpc call. The default is false */
  def lockUnspent: Option[Boolean]

  /**
   * Reserves the change output key from the keypool. The default is true. Before 0.14.0, the used keypool
   * key was never marked as change-address key and directly returned to the keypool (leading to address reuse).
   */
  def reserveChangeKey: Option[Boolean]

  /** The specific feerate you are willing to pay(BTC per KB).
    * If not set, the wallet determines the fee */
  def feeRate: Option[Bitcoins]

  def subtractFeeFromOutputs: Seq[Int]
}

object FundRawTransactionOptions {
  private case class FundRawTransactionOptionsImpl(changeAddress: Option[BitcoinAddress], changePosition: Option[Int],
                                                  includeWatching: Option[Boolean], lockUnspent: Option[Boolean],
                                                  reserveChangeKey: Option[Boolean], feeRate: Option[Bitcoins],
                                                  subtractFeeFromOutputs: Seq[Int]) extends FundRawTransactionOptions

  def apply(changeAddress: Option[BitcoinAddress], changePosition: Option[Int],
            includeWatching: Option[Boolean], lockUnspent: Option[Boolean],
            reserveChangeKey: Option[Boolean], feeRate: Option[Bitcoins],
            subtractFeeFromOutputs: Seq[Int]): FundRawTransactionOptions = {
    FundRawTransactionOptionsImpl(changeAddress, changePosition,
      includeWatching, lockUnspent, reserveChangeKey, feeRate, subtractFeeFromOutputs)
  }
}
