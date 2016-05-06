package org.bitcoins.rpc.marshallers

import org.bitcoins.rpc.bitcoincore.blockchain.softforks.SoftForks
import org.bitcoins.rpc.bitcoincore.networking.{NetworkConnections, PeerInfo}
import org.bitcoins.rpc.marshallers.networking.NetworkConnectionsMarshaller.NetworkConnectionsFormatter
import org.bitcoins.rpc.marshallers.networking.PeerInfoRPCMarshaller.PeerInfoFormatter
import spray.json.{JsArray, JsValue, JsonWriter}

import scala.collection.breakOut

/**
 * Created by chris on 12/27/15.
 */
trait RPCMarshallerUtil {

  def convertToJsArray[T](seq : Seq[T])(implicit formatter : JsonWriter[T]) : JsArray  = {
    JsArray(seq.map(p =>
      formatter.write(p))(breakOut): Vector[JsValue])
  }

  def convertToPeerInfoSeq(value : JsValue) : Seq[PeerInfo] = {
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => PeerInfoFormatter.read(e))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing a list of peer info")
    }
  }

  def convertToNetworkConnectionList(value : JsValue) : Seq[NetworkConnections] = {
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => NetworkConnectionsFormatter.read(e))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing a list of network connections")
    }
  }
}
