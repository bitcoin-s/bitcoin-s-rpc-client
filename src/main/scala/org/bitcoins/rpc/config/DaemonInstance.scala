package org.bitcoins.rpc.config

import akka.http.scaladsl.model.Uri
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.auth.AuthCredentials

/**
  * Created by chris on 4/29/17.
  */
sealed trait DaemonInstance {

  def network: NetworkParameters
  def uri: Uri
  def rpcUri: Uri
  def authCredentials: AuthCredentials
}

object DaemonInstance {
  private case class DaemonInstanceImpl(network: NetworkParameters, uri: Uri, rpcUri: Uri,
                                          authCredentials: AuthCredentials) extends DaemonInstance

  def apply(network: NetworkParameters, uri: Uri, rpcUri: Uri, authCredentials: AuthCredentials): DaemonInstance = {
    DaemonInstanceImpl(network,uri,rpcUri,authCredentials)
  }
}