package org.bitcoins.rpc.util

import java.io.PrintWriter

import akka.http.scaladsl.model.Uri
import org.bitcoins.core.config.RegTest
import org.bitcoins.rpc.auth.AuthCredentials
import org.bitcoins.rpc.config.BitcoindInstance

/**
  * Created by chris on 5/2/17.
  */
trait TestUtil {

  def randomDirName: String = 0.until(5).map(_ => scala.util.Random.alphanumeric.head).mkString

  /** Creates a datadir and places the username/password combo
    * in the bitcoin.conf in the datadir */
  def authCredentials: AuthCredentials  = {
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
    pw.close()
    AuthCredentials(username,pass,d)
  }

  lazy val network = RegTest
  /** Test instance for [[BitcoindInstance]] -- this is connected to RegTest on your machine */
  lazy val instance = BitcoindInstance(network,Uri("http://localhost:" + network.rpcPort),authCredentials)
}

object TestUtil extends TestUtil
