package org.bitcoins.rpc.bitcoincore.wallet

import org.bitcoins.rpc.bitcoincore.CoreError

/**
  * Created by chris on 5/16/17.
  * [[https://bitcoin.org/en/developer-reference#importmulti]]
  */
sealed trait ImportMultiResponse {

  def success: Boolean

  def error: Option[CoreError]

}


object ImportMultiResponse {
  private case class ImportMultiResponseImpl(success: Boolean, error: Option[CoreError]) extends ImportMultiResponse

  def apply(success: Boolean, error: Option[CoreError]): ImportMultiResponse = {
    ImportMultiResponseImpl(success,error)
  }
}
