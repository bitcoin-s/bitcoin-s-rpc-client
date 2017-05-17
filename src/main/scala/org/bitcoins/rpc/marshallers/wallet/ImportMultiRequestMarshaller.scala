package org.bitcoins.rpc.marshallers.wallet

import org.bitcoins.core.config.{MainNet, RegTest}
import org.bitcoins.core.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.core.protocol.{BitcoinAddress, CompactSizeUInt}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.rpc.bitcoincore.wallet.ImportMultiRequest
import spray.json.{DefaultJsonProtocol, JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

/**
  * Created by chris on 5/16/17.
  */
object ImportMultiRequestMarshaller extends DefaultJsonProtocol {
  val scriptPubKeyKey = "scriptPubKey"
  val addressKey = "address"
  val timestampKey = "timestamp"
  val redeemScriptKey = "redeemscript"
  val pubkeysKey = "pubkeys"
  val keysKey = "keys"
  val internalKey = "internal"
  val watchOnlyKey = "watchonly"
  val labelKey = "label"
  implicit object ImportMultiRequestMarshaller extends RootJsonFormat[ImportMultiRequest] {
    override def read(value: JsValue): ImportMultiRequest = {
      val f = value.asJsObject.fields
      val scriptOrAddress = f(scriptPubKeyKey)
      val result = if (scriptOrAddress.isInstanceOf[JsObject]) {
        val obj = scriptOrAddress.asJsObject
        Right(BitcoinAddress(obj.fields(addressKey).convertTo[String]))
      } else {
        val asmHex = scriptOrAddress.convertTo[String]
        Left(ScriptPubKey(CompactSizeUInt.calculateCompactSizeUInt(asmHex).hex + asmHex))
      }
      val timestamp = f(timestampKey).convertTo[Option[Long]]
      val redeemScript = f(redeemScriptKey).convertTo[Option[String]].map { asmHex =>
        val cmpct = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
        ScriptPubKey(cmpct.hex + asmHex)
      }
      val pubKeys = f(pubkeysKey).convertTo[Vector[String]].map(ECPublicKey(_))
      val keys = f(keysKey).convertTo[Vector[String]].map(ECPrivateKey.fromWIFToPrivateKey(_))
      val internal = f(internalKey).convertTo[Boolean]
      val watchOnly = f(watchOnlyKey).convertTo[Boolean]
      val label = f(labelKey).convertTo[String]
      //TODO: This is a bug, need to figure out how to actually derive the network from WIF keys
      val network = MainNet
      //if internal is set, we cannot have a label
      //if the impormult is not set it must be an address, NOT a spk
      if (internal) {
        ImportMultiRequest(result,timestamp,redeemScript,pubKeys,keys,internal,watchOnly, network)
      } else {
        ImportMultiRequest(result.right.get,timestamp,redeemScript,pubKeys,keys,watchOnly,label,network)
      }
    }

    /** The "now" means we start monitoring the blockchain from the last block timestamp if we did not
      * specify an actual timestamp */
    override def write(request: ImportMultiRequest): JsValue = {
      val isAddress = request.scriptOrAddress.isRight
      val addr: Option[JsObject] = if (isAddress) {
        Some(JsObject(Map(addressKey -> JsString(request.scriptOrAddress.right.get.value))))
      } else None
      val m: Map[String, JsValue] = Map(
        scriptPubKeyKey -> (if (addr.isDefined) addr.get
        else JsString(BitcoinSUtil.encodeHex(request.scriptOrAddress.left.get.asmBytes))),
        timestampKey -> ( if (request.timestamp.isDefined) JsNumber(request.timestamp.get) else JsString("now")),
        keysKey -> JsArray(request.keys.map(k => JsString(k.toWIF(request.network))).toVector),
        pubkeysKey -> JsArray(request.pubKeys.map(k => JsString(k.hex)).toVector),
        internalKey -> JsBoolean(request.internal),
        watchOnlyKey -> JsBoolean(request.watchOnly)
      )

      val withRedeemScript: Map[String,JsValue] = if (request.redeemScript.isDefined) {
        m.updated(redeemScriptKey,JsString(BitcoinSUtil.encodeHex(request.redeemScript.get.asmBytes)))
      } else m
      val withLabel = if (!request.internal && request.label.nonEmpty) {
        withRedeemScript.updated(labelKey, JsString(request.label))
      } else withRedeemScript
      JsObject(withLabel)
    }
  }
}
