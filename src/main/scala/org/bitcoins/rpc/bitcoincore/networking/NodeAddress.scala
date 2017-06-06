package org.bitcoins.rpc.bitcoincore.networking

import akka.http.scaladsl.model.Uri

/**
  * Created by chris on 5/7/17.
  */
sealed trait NodeAddress {

  def address: Uri
  def connected: ConnectionType
}

object NodeAddress {
  private case class NodeAddressImpl(address: Uri, connected: ConnectionType) extends NodeAddress

  def apply(address: Uri, connectionType: ConnectionType): NodeAddress = {
    NodeAddressImpl(address,connectionType)
  }
}
