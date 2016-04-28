package org.bitcoins.marshallers

import org.bitcoins.marshallers.rpc.bitcoincore.blockchain.softforks.SoftForkMarshaller.SoftForkFormatter
import org.bitcoins.marshallers.rpc.bitcoincore.networking.NetworkConnectionsMarshaller.NetworkConnectionsFormatter
import org.bitcoins.marshallers.rpc.bitcoincore.networking.PeerInfoMarshaller.PeerInfoFormatter
import org.bitcoins.marshallers.transaction.TransactionInputMarshaller.TransactionInputFormatter
import org.bitcoins.marshallers.transaction.TransactionOutputMarshaller.TransactionOutputFormatter
import org.bitcoins.protocol.BitcoinAddress
import org.bitcoins.rpc.bitcoincore.blockchain.softforks.SoftForks
import org.bitcoins.rpc.bitcoincore.networking.{NetworkConnections, PeerInfo}
import org.bitcoins.protocol.transaction.{TransactionOutput, TransactionInput}
import spray.json.{JsonWriter, JsArray, DefaultJsonProtocol, JsValue}
import scala.collection.breakOut

/**
 * Created by chris on 12/27/15.
 */
trait MarshallerUtil {

  def convertToJsArray[T](seq : Seq[T])(implicit formatter : JsonWriter[T]) : JsArray  = {
    JsArray(seq.map(p =>
      formatter.write(p))(breakOut): Vector[JsValue])
  }


  def convertToTransactionInputList(value : JsValue) : Seq[TransactionInput] = {
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => TransactionInputFormatter.read(e))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing a list of transaction inputs")
    }
  }

  def convertToTransactionOutputList(value : JsValue) : Seq[TransactionOutput] = {
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => TransactionOutputFormatter.read(e))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing a list of transaction inputs")
    }
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

  def convertToSoftForksList(value : JsValue) : Seq[SoftForks] = {
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => SoftForkFormatter.read(e))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing softforks info")
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
