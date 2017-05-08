package org.bitcoins.rpc.bitcoincore.networking

import akka.http.scaladsl.model.Uri

/**
  * Created by chris on 5/7/17.
  */
sealed trait AddedNodeInfo {
  def addedNode: Uri
  def connected: Boolean
  def addresses: Seq[NodeAddress]
}


object AddedNodeInfo {
  private case class AddedNodeInfoImpl(addedNode: Uri, connected: Boolean, addresses: Seq[NodeAddress]) extends AddedNodeInfo

  def apply(addedNode: Uri, connected: Boolean, addresses: Seq[NodeAddress]): AddedNodeInfo = {
    AddedNodeInfoImpl(addedNode,connected,addresses)
  }
}