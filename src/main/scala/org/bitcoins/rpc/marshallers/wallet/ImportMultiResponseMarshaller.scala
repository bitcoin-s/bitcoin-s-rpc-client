package org.bitcoins.rpc.marshallers.wallet

import org.bitcoins.rpc.bitcoincore.CoreError
import org.bitcoins.rpc.bitcoincore.wallet.ImportMultiResponse
import spray.json._
import org.bitcoins.rpc.marshallers.CoreErrorMarshaller._
/**
  * Created by chris on 5/16/17.
  */
object ImportMultiResponseMarshaller extends DefaultJsonProtocol {

  val successKey = "success"
  val errorKey = "error"

  implicit object ImportMultiResponseFormatter extends RootJsonFormat[ImportMultiResponse] {
    override def read(value: JsValue): ImportMultiResponse = {

      val f = value.asJsObject.fields
      val success = f(successKey).convertTo[Boolean]
      val error = if (f.keys.exists(_ == errorKey)) {
        Some(f(errorKey).convertTo[CoreError])
      } else None
      ImportMultiResponse(success,error)
    }

    override def write(response: ImportMultiResponse): JsValue = {
      val m = Map(
        successKey -> JsBoolean(response.success),
        errorKey -> response.error.toJson
      )
      JsObject(m)
    }
  }
}
