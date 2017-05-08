package org.bitcoins.rpc.marshallers.networking

import java.net.InetSocketAddress

import akka.http.scaladsl.model.Uri
import org.bitcoins.rpc.bitcoincore.networking.{ConnectionType, NodeAddress}
import spray.json.{DefaultJsonProtocol, JsObject, JsString, JsValue, RootJsonFormat}

/**
  * Created by chris on 5/7/17.
  */
object NodeAddressMarshaller extends DefaultJsonProtocol {
  val addressKey = "address"
  val connectedKey = "connected"
  implicit object NodeAddressFormatter extends RootJsonFormat[NodeAddress] {

    override def read(value: JsValue): NodeAddress = {
      val obj = value.asJsObject
      val f = obj.fields
      val arr = f(addressKey).convertTo[String].split(":")
      val (host,port) = (arr.head, arr(1))
      val socketAddress = new InetSocketAddress(host,port.toInt)
      val address = Uri(socketAddress.toString)
      val connected = ConnectionType(f(connectedKey).convertTo[String])
      NodeAddress(address,connected.get)
    }

    override def write(address: NodeAddress): JsValue = {
      val m = Map(
        addressKey -> JsString(address.address.toString()),
        connectedKey -> JsString(address.connected.label)
      )
      JsObject(m)
    }
  }
}
