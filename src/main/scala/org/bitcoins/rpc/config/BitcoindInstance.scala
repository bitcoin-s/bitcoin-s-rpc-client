package org.bitcoins.rpc.config

import akka.http.scaladsl.model.Uri
import org.bitcoins.core.config.NetworkParameters

/**
  * Created by chris on 4/29/17.
  */
sealed trait BitcoindInstance {

  def network: NetworkParameters

  def uri: Uri
}

object BitcoindInstance {
  private case class BitcoindInstanceImpl(network: NetworkParameters, uri: Uri) extends BitcoindInstance

  def apply(network: NetworkParameters, uri: Uri): BitcoindInstance = {
    BitcoindInstanceImpl(network,uri)
  }
}