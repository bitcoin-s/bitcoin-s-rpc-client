package org.bitcoins.rpc.util

import java.io.PrintWriter

import akka.http.scaladsl.model.Uri
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.gen.{ScriptGenerators, TransactionGenerators}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{CSVScriptPubKey, LockTimeScriptPubKey}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.RPCClient
import org.bitcoins.rpc.auth.AuthCredentials
import org.bitcoins.rpc.channels.{ChannelClient, ChannelServer}
import org.bitcoins.rpc.config.DaemonInstance

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by chris on 5/2/17.
  */
trait TestUtil extends BitcoinSLogger {

  def randomDirName: String = 0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString

  /** Creates a datadir and places the username/password combo
    * in the bitcoin.conf in the datadir */
  def authCredentials(uri: Uri, rpcUri: Uri): AuthCredentials  = {
    val d = "/tmp/" + randomDirName
    val f = new java.io.File(d)
    f.mkdir()
    val conf = new java.io.File(f.getAbsolutePath + "/bitcoin.conf")
    conf.createNewFile()
    val username = "random_user_name"
    val pass = randomDirName
    val pw = new PrintWriter(conf)
    pw.write("rpcuser=" + username + "\n")
    pw.write("rpcpassword=" + pass + "\n")
    pw.write("rpcbind=" + rpcUri.toString + "\n")
    pw.write("rpcport=" + rpcUri.effectivePort + "\n")
    pw.write("port=" + uri.effectivePort + "\n")
    //pw.write("txindex=1")
    pw.close()
    AuthCredentials(username,pass,d)
  }

  lazy val network = RegTest

  /** Test instance for [[DaemonInstance]] -- this is connected to RegTest on your machine */
  def instance(port: Int, rpcPort: Int): DaemonInstance = {
    val uri = Uri("http://localhost:" + port)
    val rpcUri = Uri("http://localhost:" + rpcPort)
    DaemonInstance(network,uri,rpcUri,authCredentials(uri,rpcUri))
  }

  def startNodes(clients: Seq[RPCClient]): Unit = {
    clients.map(_.start)
    Thread.sleep(60000)
  }

  def connectTwoNodes(node1: RPCClient, node2: RPCClient)(implicit ec: ExecutionContext): Future[Unit] = {
    val added: Future[Unit] = node1.addNode(node2.instance.uri)
    added.flatMap(_ => node2.addNode(node1.instance.uri))
  }

  def stopNodes(clients: Seq[RPCClient])(implicit ec: ExecutionContext): Future[Unit] = {
    Future.sequence(clients.map(_.stop)).map(_.head)
  }

  def randomPort: Int = {
    val firstAttempt = Math.abs(scala.util.Random.nextInt % 15000)
    if (firstAttempt < 2000) {
      firstAttempt + 2000
    } else firstAttempt
  }

  /** Gives us a [[Channel]] with two connected nodes where the client has initialized the
    * payment channel with [[Policy.minChannelAmount]]
    */
  def initializedChannel(implicit materializer: ActorMaterializer): Future[(ChannelClient, ChannelServer)] = {
    implicit val dispatcher = materializer.system.dispatcher
    val node1 = TestUtil.instance(randomPort, randomPort)
    val client1 = RPCClient(node1,materializer)
    val node2 = TestUtil.instance(randomPort, randomPort)
    val client2 = RPCClient(node2,materializer)

    val started = TestUtil.startNodes(Seq(client1,client2))
    //need to generate 475 blocks for segwit to activate on regtest
    val connected = TestUtil.connectTwoNodes(client1,client2)
    val generateBlocks: Future[Seq[DoubleSha256Digest]] = connected.flatMap { _ =>
      client1.generate(300)
    }

    val generateBlocksClient2: Future[Seq[DoubleSha256Digest]] = generateBlocks.flatMap(_ => client2.generate(175))

    val serverPrivKey = ECPrivateKey()
    val serverPubKey = serverPrivKey.publicKey
    val p2pkh = client1.getNewAddress
    val unspendable = ScriptNumber(100)
    val lockTimeScriptPubKey = p2pkh.map(p => CSVScriptPubKey(unspendable,p.scriptPubKey))
    val changeClientSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1

    val pcClient: Future[ChannelClient] = lockTimeScriptPubKey.flatMap { lockTimeSPK =>
      generateBlocksClient2.flatMap { _ =>
        ChannelClient(client1, serverPubKey, lockTimeSPK, CurrencyUnits.oneBTC)
      }
    }
    val generatedBlocks = pcClient.flatMap { _ =>
      client1.generate(10)
    }

    val pcServer: Future[ChannelServer] = generatedBlocks.flatMap { _ =>
      pcClient.flatMap { cli =>
        Thread.sleep(7500)
        ChannelServer(client2, serverPrivKey, cli.channel.anchorTx.txId, cli.channel.lock)
      }
    }
    val amount = Policy.minChannelAmount
    val clientSigned: Future[(ChannelClient,Transaction)] = generatedBlocks.flatMap { _ =>
      pcClient.flatMap(pc => pc.update(changeClientSPK, amount))
    }

    val pcServerUpdated: Future[ChannelServer] = clientSigned.flatMap { cli =>
      pcServer.flatMap { server =>
        server.update(cli._2,changeClientSPK)
      }
    }
    clientSigned.flatMap(pc => pcServerUpdated.map(ps => (pc._1,ps)))
  }
}

object TestUtil extends TestUtil
