package org.bitcoins.rpc

import akka.http.javadsl.model.headers.HttpCredentials
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import org.bitcoins.rpc.config.DaemonInstance
import spray.json.{JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue}

import scala.concurrent.Future

/**
  * Created by chris on 4/25/17.
  */
sealed trait RPCHandler {

  def sendRequest(instance: DaemonInstance, jsObject: JsObject)(implicit m: ActorMaterializer): Future[HttpResponse] = {
    val username = instance.authCredentials.username
    val password = instance.authCredentials.password
    val req = HttpRequest(method = HttpMethods.POST, uri = instance.rpcUri,
      entity = HttpEntity(ContentTypes.`application/json`,jsObject.toString()))
      .addCredentials(HttpCredentials.createBasicHttpCredentials(username,password))
    sendRequest(req)
  }

  def sendRequest(req: HttpRequest)(implicit m: ActorMaterializer): Future[HttpResponse] = {
    Http(m.system).singleRequest(req)
  }

  def buildRequest(methodName: String, param: Int): JsObject = {
    buildRequest(methodName, JsArray(JsNumber(param)))
  }

  def buildRequest(methodName: String, param: String): JsObject = {
    buildRequest(methodName,JsArray(JsString(param)))
  }

  def buildRequest(methodName: String, param: Boolean): JsObject = {
    buildRequest(methodName, JsArray(JsBoolean(param)))
  }

  def buildRequest(methodName: String, params: JsArray): JsObject = {
    val m : Map[String,JsValue] = Map(
      "method" -> JsString(methodName),
      "params" -> params,
      "id" -> JsString(""))
    JsObject(m)
  }

  def buildRequest(methodName: String): JsObject = {
    buildRequest(methodName,JsArray(Vector()))
  }

  def buildRequest(methodName: String, param1: String, opt: JsObject): JsObject = {
    val arr = JsArray(JsString(param1),opt)
    buildRequest(methodName,arr)
  }
}

object RPCHandler extends RPCHandler
