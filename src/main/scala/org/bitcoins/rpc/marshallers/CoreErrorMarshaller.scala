package org.bitcoins.rpc.marshallers

import org.bitcoins.rpc.bitcoincore.CoreError
import spray.json.{DefaultJsonProtocol, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

/**
  * Created by chris on 5/17/17.
  */
object CoreErrorMarshaller extends DefaultJsonProtocol {
  val codeKey = "code"
  val messageKey = "message"
  implicit object CoreErrorFormatter extends RootJsonFormat[CoreError] {

    override def read(value: JsValue): CoreError = {
      val f = value.asJsObject.fields
      val code = f(codeKey).convertTo[Int]
      val message = f(messageKey).convertTo[String]
      CoreError(code,message)
    }

    override def write(error: CoreError): JsValue = {
      val m = Map(
        codeKey -> JsNumber(error.code),
        messageKey -> JsString(error.message)
      )
      JsObject(m)
    }
  }
}
