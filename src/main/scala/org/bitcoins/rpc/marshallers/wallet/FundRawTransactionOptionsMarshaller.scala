package org.bitcoins.rpc.marshallers.wallet

import org.bitcoins.rpc.bitcoincore.wallet.FundRawTransactionOptions
import spray.json.{DefaultJsonProtocol, JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

/**
  * Created by chris on 5/19/17.
  */
object FundRawTransactionOptionsMarshaller extends DefaultJsonProtocol {
  val changeAddressKey = "changeAddress"
  val changePositionKey = "changePosition"
  val includeWatchingKey = "includeWatching"
  val lockUnspentKey = "lockUnspent"
  val reserveChangeKeyKey = "reserveChangeKey"
  val feeRateKey = "feeRate"
  val subtractFeeFromOutputsKey = "subtractFeeFromOutputs"

  implicit object FundRawTransactionOptionsFormatter extends RootJsonFormat[FundRawTransactionOptions] {
    override def read(value: JsValue): FundRawTransactionOptions = {
      //TODO: implement
      ???
    }

    override def write(options: FundRawTransactionOptions): JsValue = {
      val withChangeAddress: Map[String,JsValue] = if (options.changeAddress.isDefined) {
        Map(changeAddressKey -> JsString(options.changeAddress.get.value))
      } else Map()

      val withChangePos = if (options.changePosition.isDefined) {
        withChangeAddress.updated(changePositionKey, JsNumber(options.changePosition.get))
      } else withChangeAddress

      val withIncludeWatchingKey = if (options.includeWatching.isDefined) {
        withChangePos.updated(includeWatchingKey,JsBoolean(options.includeWatching.get))
      } else withChangePos

      val withLockUnspent = if (options.lockUnspent.isDefined) {
        withIncludeWatchingKey.updated(lockUnspentKey,JsBoolean(options.lockUnspent.get))
      } else withIncludeWatchingKey

      val withReserveChangeKey = if (options.reserveChangeKey.isDefined) {
        withLockUnspent.updated(reserveChangeKeyKey, JsBoolean(options.reserveChangeKey.get))
      } else withLockUnspent

      val withFeeRate = if (options.feeRate.isDefined) {
        withReserveChangeKey.updated(feeRateKey, JsNumber(options.feeRate.get.toBigDecimal))
      } else withReserveChangeKey

      val withSubtractFeeFromOutputs = {
        val arr = JsArray(options.subtractFeeFromOutputs.map(JsNumber(_)).toVector)
        withFeeRate.updated(subtractFeeFromOutputsKey, arr)
      }
      JsObject(withSubtractFeeFromOutputs)
    }
  }
}
