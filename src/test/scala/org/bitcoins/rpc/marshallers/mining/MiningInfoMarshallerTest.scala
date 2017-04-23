package org.bitcoins.rpc.marshallers.mining

import org.bitcoins.rpc.bitcoincore.mining.GetMiningInfo
import org.scalatest.{MustMatchers, FlatSpec}
import spray.json._

/**
 * Created by Tom on 1/13/2016.
 */
class MiningInfoMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      |{
      |    "blocks" : 633027,
      |    "currentblocksize" : 0,
      |    "currentblocktx" : 0,
      |    "difficulty" : 1.00000000,
      |    "errors" : "",
      |    "genproclimit" : -1,
      |    "networkhashps" : 2302003138855,
      |    "pooledtx" : 78,
      |    "testnet" : true,
      |    "chain" : "test",
      |    "generate" : false
      |}
    """.stripMargin

  val json = str.parseJson
  val miningMeta : GetMiningInfo = MiningInfoMarshaller.MiningInfoFormatter.read(json)

  "MiningInfoMarshaller" must "parse mining information" in {
    miningMeta.blocks must be (633027)
    miningMeta.currentBlockSize must be (0)
    miningMeta.currentBlockTx must be (0)
    miningMeta.difficulty must be (1.00000000)
    miningMeta.errors must be ("")
    miningMeta.networkHashPerSecond must be (BigInt("2302003138855"))
    miningMeta.pooledTx must be (78)
    miningMeta.chain must be ("test")
  }

  it must "write mining info" in {
    val writtenMining = MiningInfoMarshaller.MiningInfoFormatter.write(miningMeta)
    writtenMining.asJsObject.fields("blocks") must be (JsNumber(633027))
    writtenMining.asJsObject.fields("currentblocksize") must be (JsNumber(0))
    writtenMining.asJsObject.fields("currentblocktx") must be (JsNumber(0))
    writtenMining.asJsObject.fields("difficulty") must be (JsNumber(1.00000000))
    writtenMining.asJsObject.fields("errors") must be (JsString(""))
    writtenMining.asJsObject.fields("networkhashps") must be (JsNumber(BigInt("2302003138855")))
    writtenMining.asJsObject.fields("pooledtx") must be (JsNumber(78))
    writtenMining.asJsObject.fields("chain") must be (JsString("test"))
  }
}
