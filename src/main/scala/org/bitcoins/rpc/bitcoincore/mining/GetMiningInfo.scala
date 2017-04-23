package org.bitcoins.rpc.bitcoincore.mining

/**
 * Created by Tom on 1/13/2016.
 */
trait GetMiningInfo {
  def blocks : Int
  def currentBlockSize : Int
  def currentBlockTx : Int
  def difficulty : Double
  def errors : String
  def networkHashPerSecond : BigInt
  def pooledTx : Int
  def chain : String
}

case class GetMiningInfoImpl(blocks : Int, currentBlockSize : Int, currentBlockTx : Int, difficulty : Double, errors : String,
                              networkHashPerSecond : BigInt, pooledTx : Int,
                              chain : String) extends  GetMiningInfo
