package org.bitcoins.rpc.marshallers.mining

import org.bitcoins.rpc.bitcoincore.mining.{GetMiningInfo, GetMiningInfoImpl}
import spray.json._

/**
 * Created by Tom on 1/13/2016.
 */
object MiningInfoMarshaller extends DefaultJsonProtocol {
  val blocksKey = "blocks"
  val currentBlockSizeKey = "currentblocksize"
  val currentBlockTxKey = "currentblocktx"
  val difficultyKey = "difficulty"
  val errorsKey = "errors"
  val networkHashPerSecondKey = "networkhashps"
  val pooledTxKey = "pooledtx"
  val chainKey = "chain"
  val generateKey = "generate"

  implicit object MiningInfoFormatter extends RootJsonFormat[GetMiningInfo] {
    override def read(value : JsValue) : GetMiningInfo = {
      val obj = value.asJsObject
      val blocks = obj.fields(blocksKey).convertTo[Int]
      val currentBlockSize = obj.fields(currentBlockSizeKey).convertTo[Int]
      val currentBlockTx = obj.fields(currentBlockTxKey).convertTo[Int]
      val difficulty = obj.fields(difficultyKey).convertTo[Double]
      val errors = obj.fields(errorsKey).convertTo[String]
      val networkHashPerSecond = obj.fields(networkHashPerSecondKey).convertTo[BigInt]
      val pooledTx = obj.fields(pooledTxKey).convertTo[Int]
      val chain = obj.fields(chainKey).convertTo[String]
      GetMiningInfoImpl(blocks, currentBlockSize,currentBlockTx,difficulty,errors,networkHashPerSecond,
        pooledTx,chain)
    }

    override def write(miningMeta : GetMiningInfo) : JsValue = {
      val m : Map[String, JsValue] = Map (
      blocksKey -> JsNumber(miningMeta.blocks),
      currentBlockSizeKey -> JsNumber(miningMeta.currentBlockSize),
      currentBlockTxKey -> JsNumber(miningMeta.currentBlockTx),
      difficultyKey -> JsNumber(miningMeta.difficulty),
      errorsKey -> JsString(miningMeta.errors),
      networkHashPerSecondKey -> JsNumber(miningMeta.networkHashPerSecond),
      pooledTxKey -> JsNumber(miningMeta.pooledTx),
      chainKey -> JsString(miningMeta.chain)
      )
      JsObject(m)
    }
  }
}
