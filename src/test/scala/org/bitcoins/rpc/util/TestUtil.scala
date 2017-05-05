package org.bitcoins.rpc.util

import java.io.PrintWriter

import akka.http.scaladsl.model.Uri
import org.bitcoins.core.config.RegTest
import org.bitcoins.rpc.auth.AuthCredentials
import org.bitcoins.rpc.config.DaemonInstance

/**
  * Created by chris on 5/2/17.
  */
trait TestUtil {

  def randomDirName: String = 0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString

  /** Creates a datadir and places the username/password combo
    * in the bitcoin.conf in the datadir */
  def authCredentials(uri: Uri): AuthCredentials  = {
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
    pw.write("rpcbind=" + uri.toString + "\n")
    pw.close()
    AuthCredentials(username,pass,d)
  }

  lazy val network = RegTest
  /** Test instance for [[DaemonInstance]] -- this is connected to RegTest on your machine */
  def instance(rpcPort: Int): DaemonInstance = {
    val uri = Uri("http://localhost:" + rpcPort)
    DaemonInstance(network,uri,authCredentials(uri))
  }
}

object TestUtil extends TestUtil
