package org.bitcoins.rpc.marshallers.wallet

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.rpc.bitcoincore.wallet.UTXO
import spray.json.{DefaultJsonProtocol, JsBoolean, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

/**
  * Created by chris on 4/26/17.
  */
object UTXOMarshaller extends DefaultJsonProtocol {

  val txIdKey = "txid"
  val voutKey = "vout"
  val addressKey = "address"
  val scriptPubKeyKey = "scriptPubKey"
  val amountKey = "amount"
  val confirmationsKey = "confirmations"
  val spendableKey = "spendable"
  val solvableKey = "solvable"
  implicit object UTXOFormatter extends RootJsonFormat[UTXO]  {

    override def read(value: JsValue): UTXO = {
      val f = value.asJsObject.fields
      val txId = DoubleSha256Digest(f(txIdKey).convertTo[String])
      val vout = f(voutKey).convertTo[Int]
      val address = BitcoinAddress(f(addressKey).convertTo[String])
      val scriptPubKey = ScriptPubKey(f(scriptPubKeyKey).convertTo[String])
      val amount = Bitcoins(f(amountKey).convertTo[Double])
      val confirmations = f(confirmationsKey).convertTo[Int]
      val spendable = f(spendableKey).convertTo[Boolean]
      val solvable = f(solvableKey).convertTo[Boolean]
      UTXO(txId, vout, address, scriptPubKey, amount, confirmations, spendable, solvable)
    }

    override def write(utxo: UTXO): JsValue = {
      val m: Map[String,JsValue] = Map(
        txIdKey -> JsString(utxo.txId.hex),
        voutKey -> JsNumber(utxo.vout),
        scriptPubKeyKey -> JsString(utxo.scriptPubKey.hex),
        amountKey -> JsNumber(-1),
        confirmationsKey -> JsNumber(utxo.confirmations),
        spendableKey -> JsBoolean(utxo.spendable),
        solvableKey -> JsBoolean(utxo.solvable)
      )
      JsObject(m)
    }
  }
}
