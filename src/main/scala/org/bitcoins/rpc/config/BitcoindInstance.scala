package org.bitcoins.rpc.config

import akka.http.scaladsl.model.Uri
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.auth.AuthCredentials

/**
  * Created by chris on 4/29/17.
  */
sealed trait BitcoindInstance {

  def network: NetworkParameters
  def uri: Uri
  def authCredentials: AuthCredentials
}

object BitcoindInstance {
  private case class BitcoindInstanceImpl(network: NetworkParameters, uri: Uri,
                                          authCredentials: AuthCredentials) extends BitcoindInstance

  def apply(network: NetworkParameters, uri: Uri, authCredentials: AuthCredentials): BitcoindInstance = {
    BitcoindInstanceImpl(network,uri,authCredentials)
  }
}