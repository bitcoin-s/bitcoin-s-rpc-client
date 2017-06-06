package org.bitcoins.rpc.bitcoincore.networking

/**
  * Created by chris on 5/7/17.
  */
sealed trait ConnectionType {
  def label: String
}

case object Disconnected extends ConnectionType {
  override def label = "false"
}
case object InboundConnection extends ConnectionType {
  override def label = "inbound"
}
case object OutboundConnection extends ConnectionType {
  override def label = "outbound"
}

object ConnectionType {
  private val all: Seq[ConnectionType] = Seq(Disconnected, InboundConnection, OutboundConnection)

  def apply(label: String): Option[ConnectionType] = all.find(_.label == label)
}
