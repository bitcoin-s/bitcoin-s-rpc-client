package org.bitcoins.rpc.bitcoincore.wallet

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey

/**
  * Created by chris on 4/26/17.
  * Represents this data structure from bitcoin core
  * [[https://bitcoin.org/en/developer-reference#listunspent]]
  */
sealed trait UTXO {

  def txId: DoubleSha256Digest
  def vout: Int
  def address: BitcoinAddress
  def scriptPubKey: ScriptPubKey
  def amount: CurrencyUnit
  def confirmations: Int
  def spendable: Boolean

  /** Set to true if the wallet knows how to spend this output.
    * Set to false if the wallet does not know how to spend the output.
    * It is ignored if the private keys are available */
  def solvable: Boolean
}

object UTXO {
  private case class UTXOImpl(txId: DoubleSha256Digest, vout: Int, address: BitcoinAddress, scriptPubKey: ScriptPubKey,
                              amount: CurrencyUnit, confirmations: Int, spendable: Boolean, solvable: Boolean) extends UTXO


  def apply(txId: DoubleSha256Digest, vout: Int, address: BitcoinAddress, scriptPubKey: ScriptPubKey,
            amount: CurrencyUnit, confirmations: Int, spendable: Boolean, solvable: Boolean): UTXO  = {
    UTXOImpl(txId, vout, address, scriptPubKey, amount, confirmations, spendable, solvable)
  }
}