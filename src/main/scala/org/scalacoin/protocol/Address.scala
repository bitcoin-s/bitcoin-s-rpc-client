package org.scalacoin.protocol

import org.bitcoinj.core.{VersionedChecksummedBytes, Base58, Utils}

case class AddressInfo(bitcoinAddress: BitcoinAddress, n_tx: Long, total_received: Long, total_sent: Long,
  final_balance: Long)

sealed abstract class Address(val address : String)
sealed case class BitcoinAddress(bitcoinAddress: String) extends Address(bitcoinAddress) {
  require(BitcoinAddress.validate(bitcoinAddress), "Bitcoin address was invalid " + bitcoinAddress)
}

sealed case class AssetAddress(assetAddress : String) extends Address(assetAddress) {
  require(AssetAddress.validate(assetAddress), "The provided asset was was invalid: " + assetAddress)
}

object BitcoinAddress {
  def validate(bitcoinAddress: String): Boolean = {
    val illegalChars = List('O', 'I', 'l', '0')
    bitcoinAddress.length >= 26 && bitcoinAddress.length <= 35 &&
      (p2pkh(bitcoinAddress) || p2shAddress(bitcoinAddress)) &&
      bitcoinAddress.filter(c => illegalChars.contains(c)).size == 0
  }

  /**
   * Converts a bitcoin address to an asset address
   * @param address
   * @return
   */
  def convertToAssetAddress(address : BitcoinAddress) : AssetAddress = {
    val underlying : String  = address.bitcoinAddress
    val base58decodeChecked : Array[Byte] = Base58.decodeChecked(underlying)
    require (
      base58decodeChecked.size == 21
    )
    AssetAddress(new VersionedChecksummedBytes(0x13, base58decodeChecked){}.toString())
  }

  /**
   * Checks if a address is a valid p2sh address
   * @param address
   * @return
   */
  def p2shAddress(address : String) : Boolean = {
    address.charAt(0) == '3' || address.charAt(0) == '2'
  }

  /**
   * Checks if a address is a valid p2sh address
   * @param address
   * @return
   */
  def p2shAddress(address : BitcoinAddress) : Boolean = p2shAddress(address.bitcoinAddress)

  /**
   * Checks if an address is a valid p2pkh address
   * @param address
   * @return
   */
  def p2pkh(address : String) : Boolean = {
    val firstChar = address.charAt(0)
    firstChar == '1' || firstChar == 'm' || firstChar == 'n'
  }

  /**
   * Checks if an address is a valid p2pkh address
   * @param address
   * @return
   */
  def p2pkh(address : BitcoinAddress) : Boolean = p2pkh(address.bitcoinAddress)
}

object AssetAddress {
  def validate(assetAddress : String) = {
    //asset addresses must have the one byte namespace equivalent to 19
    //which ends up being 'a' in the ascii character set
    val base58decodechecked : Array[Byte] = Base58.decodeChecked(assetAddress)
    require(base58decodechecked != null)
    base58decodechecked.size == 22  && base58decodechecked(0) == 0x13
  }

  /**
   * Converts an asset address into a bitcoin address
   * @param assetAddress
   * @return
   */
  def convertToBitcoinAddress(assetAddress : AssetAddress) = {
    val underlying : String = assetAddress.assetAddress
    val base58decodeChecked : Array[Byte] = Base58.decodeChecked(underlying)

    require(base58decodeChecked.size == 22)

    val slice = base58decodeChecked.slice(2, base58decodeChecked.length)
    BitcoinAddress(new VersionedChecksummedBytes(base58decodeChecked(1), slice){}.toString())
  }
}