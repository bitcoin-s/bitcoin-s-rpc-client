package org.bitcoins.rpc.bitcoincore.wallet

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey

/**
  * Created by chris on 5/16/17.
  * [[https://bitcoin.org/en/developer-reference#importmulti]]
  */
sealed trait ImportMultiRequest {

  /** Must either provide the ScriptPubKey or BitcoinAddress */
  def scriptOrAddress: Either[ScriptPubKey, BitcoinAddress]

  /** The creation time of the key in Unix epoch time or the string “now” to substitute the current synced block chain time.
    * The timestamp of the oldest key will determine how far back block chain rescans need to begin.
    * Specify now to bypass scanning for keys which are known to never have been used.
    * Specify 0 to scan the entire block chain.
    * Blocks up to 2 hours before the earliest key creation time will be scanned
    *
    * If 'None' is provided we will send "now" to bitcoin core
    * */
  def timestamp: Option[Long]

  /** A redeem script. Only allowed if either the address field is a
    * P2SH address or the scriptPubKey field is a P2SH scriptPubKey */
  def redeemScript: Option[ScriptPubKey]

  /** Array of strings giving pubkeys that must occur in the scriptPubKey or redeemscript */
  def pubKeys: Seq[ECPublicKey]

  /** Array of strings giving private keys whose corresponding public keys
    * must occur in the scriptPubKey or redeemscript */
  def keys: Seq[ECPrivateKey]
  /** Stating whether matching outputs should be treated as change rather than incoming payments.
    * The default is false */
  def internal: Boolean

  /** Stating whether matching outputs should be considered watched even when they’re not spendable.
    * This is only allowed if keys are empty. The default is false */
  def watchOnly: Boolean

  /** Label to assign to the address, only allowed with internal set to false.
    * The default is an empty string (“”) */
  def label: String



  def network: NetworkParameters
}

object ImportMultiRequest {
  private case class ImportMultiRequestImpl(scriptOrAddress: Either[ScriptPubKey, BitcoinAddress], timestamp: Option[Long],
                                           redeemScript: Option[ScriptPubKey], pubKeys: Seq[ECPublicKey], keys: Seq[ECPrivateKey],
                                           internal: Boolean, watchOnly: Boolean, label: String,
                                           network: NetworkParameters) extends ImportMultiRequest

  private def apply(scriptOrAddress: Either[ScriptPubKey, BitcoinAddress], timestamp: Option[Long],
            redeemScript: Option[ScriptPubKey], pubKeys: Seq[ECPublicKey], keys: Seq[ECPrivateKey],
            internal: Boolean, watchOnly: Boolean, label: String, network: NetworkParameters): ImportMultiRequest = {
    ImportMultiRequestImpl(scriptOrAddress, timestamp,
      redeemScript, pubKeys, keys, internal, watchOnly, label,network)
  }

  def apply(address: BitcoinAddress, timestamp: Option[Long],
            redeemScript: Option[ScriptPubKey], pubKeys: Seq[ECPublicKey], keys: Seq[ECPrivateKey],
            watchOnly: Boolean, label: String, networkParameters: NetworkParameters): ImportMultiRequest = {
    ImportMultiRequest(Right(address),timestamp,redeemScript,pubKeys,keys,
      false,watchOnly,label,networkParameters)
  }

  def apply(scriptPubKey: ScriptPubKey, timestamp: Option[Long], redeemScript: Option[ScriptPubKey],
            pubKeys: Seq[ECPublicKey], keys: Seq[ECPrivateKey], watchOnly: Boolean,
            networkParameters: NetworkParameters): ImportMultiRequest = {
    ImportMultiRequest(Left(scriptPubKey), timestamp,redeemScript,pubKeys,keys,
      true,watchOnly,"",networkParameters)
  }

  def apply(scriptOrAddress: Either[ScriptPubKey, BitcoinAddress], timestamp: Option[Long],
            redeemScript: Option[ScriptPubKey], pubKeys: Seq[ECPublicKey], keys: Seq[ECPrivateKey],
            internal: Boolean, watchOnly: Boolean, networkParameters: NetworkParameters): ImportMultiRequest = {
    ImportMultiRequest(scriptOrAddress, timestamp,
      redeemScript, pubKeys, keys, internal, watchOnly, "", networkParameters)
  }

}