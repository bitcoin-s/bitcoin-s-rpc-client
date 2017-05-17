package org.bitcoins.rpc.bitcoincore

import java.nio.charset.CoderMalfunctionError

/**
  * Created by chris on 5/17/17.
  */
sealed trait CoreError {

  def code: Int
  def message: String

}

object CoreError {
  private case class CoreErrorImpl(code: Int, message: String) extends CoreError

  def apply(code: Int, message: String): CoreError = {
    CoreErrorImpl(code,message)
  }
}
