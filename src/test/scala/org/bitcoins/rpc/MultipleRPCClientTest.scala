package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.rpc.util.TestUtil
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpec, MustMatchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

/**
  * Created by chris on 5/5/17.
  */
class MultipleRPCClientTest extends FlatSpec with MustMatchers with ScalaFutures with BeforeAndAfterAll {
  implicit val actorSystem = ActorSystem("RPCClientTest")
  implicit val dispatcher = actorSystem.dispatcher
  val materializer = ActorMaterializer()

  val node1 = TestUtil.instance(TestUtil.network.port - 5, TestUtil.network.rpcPort - 5)
  val client1 = RPCClient(node1,materializer)
  val node2 = TestUtil.instance(TestUtil.network.port + 5, TestUtil.network.rpcPort + 5)
  val client2 = RPCClient(node2,materializer)

  override def beforeAll: Unit = {
    TestUtil.startNodes(Seq(client1,client2))
    val connected = TestUtil.connectTwoNodes(client1,client2)
    Await.result(connected,10.seconds)
  }

  "MultipleRPCClientTest" must "have the same blockchain tip after generating a block" in {
    val hash: Future[DoubleSha256Digest] = client1.generate(1).map(_.head)
    val bestBlockHash: Future[DoubleSha256Digest] = hash.flatMap { _ =>
      Thread.sleep(2500)
      client2.getBestBlockHash
    }
    val hashes: Future[(DoubleSha256Digest,DoubleSha256Digest)] = bestBlockHash.flatMap { best =>
      hash.map(h => (h,best))
    }

    whenReady(hashes, timeout(5.seconds), interval(500.millis)) { hs =>
      hs._1 must be (hs._2)
    }
  }

  override def afterAll: Unit = {
    val f = TestUtil.stopNodes(Seq(client1,client2))
    materializer.shutdown()
    Await.result(f,5.seconds)
  }
}
