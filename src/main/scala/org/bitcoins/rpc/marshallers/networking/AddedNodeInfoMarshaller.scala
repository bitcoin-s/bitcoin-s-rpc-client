package org.bitcoins.rpc.marshallers.networking

import akka.http.scaladsl.model.Uri
import org.bitcoins.rpc.bitcoincore.networking.{AddedNodeInfo, NodeAddress}
import spray.json.{DefaultJsonProtocol, JsArray, JsBoolean, JsObject, JsString, JsValue, RootJsonFormat}
import NodeAddressMarshaller._
import org.bitcoins.core.util.BitcoinSLogger
/**
  * Created by chris on 5/7/17.
  */
object AddedNodeInfoMarshaller extends DefaultJsonProtocol {

  val addedNodeKey = "addednode"
  val connectedKey = "connected"
  val addressesKey = "addresses"
  implicit object AddedNodeInfoFormatter extends RootJsonFormat[AddedNodeInfo] {

    override def read(value: JsValue): AddedNodeInfo = {
      val obj = value.asJsObject
      val f = obj.fields
      val addedNode = Uri(f(addedNodeKey).convertTo[String])
      val connected = f(connectedKey).convertTo[Boolean]
      val addressesArray = f(addressesKey).asInstanceOf[JsArray]
      val addresses = addressesArray.elements.map(a => a.convertTo[NodeAddress])
      AddedNodeInfo(addedNode,connected,addresses)
    }

    override def write(nodeInfo: AddedNodeInfo): JsValue = {
      val addresses: JsArray = JsArray(nodeInfo.addresses.map(n => JsString(n.toString)).toVector)
      val m: Map[String,JsValue] = Map(
        addedNodeKey -> JsString(nodeInfo.addedNode.toString),
        connectedKey -> JsBoolean(nodeInfo.connected),
        addressesKey -> addresses
      )
      JsObject(m)
    }
  }
}
