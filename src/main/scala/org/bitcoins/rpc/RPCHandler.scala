package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.http.javadsl.model.headers.{HttpCredentials, RawRequestURI}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import org.bitcoins.core.config.NetworkParameters
import spray.json.{JsArray, JsNumber, JsObject, JsString, JsValue}

import scala.concurrent.Future

/**
  * Created by chris on 4/25/17.
  */
trait RPCHandler {


  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val rawRequest =
    """
      |  {
      |      "method": "getblockhash",
      |      "params": [0],
      |      "id": "foo"
      |  }
    """.stripMargin
  def sendRequest(userName: String, password: String, uri: Uri, jsObject: JsObject): Future[HttpResponse] = {
    val req = HttpRequest(method = HttpMethods.POST, uri = uri,
      entity = HttpEntity(ContentTypes.`application/json`,jsObject.toString()))
      .addCredentials(HttpCredentials.createBasicHttpCredentials(userName,password))
    sendRequest(req)
  }

  def sendRequest(req: HttpRequest): Future[HttpResponse] = {
    Http().singleRequest(req)
  }

  def buildRequest(methodName: String, param: Int): JsObject = {
    buildRequest(methodName, JsArray(JsNumber(param)))
  }

  def buildRequest(methodName: String, param: String): JsObject = {
    buildRequest(methodName,JsArray(JsString(param)))
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
}

object RPCHandler extends RPCHandler
