package com.olvind.crud
package server

import java.io.{InputStreamReader, InputStream, OutputStreamWriter}
import javax.servlet.http.HttpServletRequest

import com.typesafe.scalalogging.LazyLogging
import unfiltered.filter.Plan
import unfiltered.filter.request.ContextPath
import unfiltered.request.{HttpRequest, Body, Seg}
import unfiltered.response._
import upickle.default._

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

trait integrationUnfiltered extends serverEditors with integrationUpickle {
  class IntegrationUnfiltered(val editors: List[Editor]) extends Plan with LazyLogging {

    object EditorList extends Editors {
      override def tables() = editors.map(_.clientTable())

      def unapply(s: String): Option[Editor] =
        editors.find(_.clientTable().name =:= TableName(s))
    }

    final implicit class ResponseFunctionX(rf: ResponseFunction[Any]){
      //eliminate compiler warning for inferring Any when using ~>
      def ~~>(other: ResponseFunction[Any]) = rf ~> other
    }

    /**
     * Not saying it's a good idea, but if you were to deploy
     *  this publicly - you'd better start here.
     *
     * Keep in mind that the code has not been audited for security!
     */
    def authorizeUser(req:  HttpRequest[HttpServletRequest])
                     (ifOk: ⇒ ResponseFunction[Any]): ResponseFunction[Any] =
       ifOk

    final def handleRoute(body: String,
                          s:    List[String],
                          r:    Autowire.Router,
                          name: String): ResponseFunction[Any] = {

      Clock { c ⇒
        def outStr(msg: String): String =
          c.withTd(td ⇒ s"$name(${s.mkString(", ")}): $msg (in ${td.toMillis} ms)")

        Try(read[Map[String, String]](body)) match {
          case Success(parsed) ⇒
            val f: Future[String] = r(autowire.Core.Request(s, parsed))
            Try(Await.result(f, Duration.Inf)) match {
              case Success(result) ⇒
                logger.debug(outStr(s"Succeeded with params $parsed"))
                JsonContent ~~> ResponseString(result)
              case Failure(th)     ⇒
                logger.warn(outStr(s"Failed with params $parsed"), th)
                BadRequest
            }
          case Failure(th) ⇒
            logger.warn(outStr(s"Failed: couldn't parse args $body"), th)
            BadRequest
        }
      }
    }

    val scripts = Seq("crud-deps.js", "crud-opt.js", "crud-opt.js.map", "crud-launcher.js")

    case class ResponseStream(is: InputStream) extends ResponseWriter {
      def write(writer: OutputStreamWriter): Unit = {
        val isr  = new InputStreamReader(is, "UTF-8")
        val a    = Array.ofDim[Char](10000)
        var read = 0
        while ({read = isr.read(a); read != -1}){
          writer.write(a, 0, read)
        }

        is.close()
      }
    }

    val intent: Plan.Intent = {
      case req@ContextPath(_, Seg(Nil)) ⇒
        authorizeUser(req) {
          Html5(
            <html>
              <body>
                <div id="app"></div>
                <link href='http://fonts.googleapis.com/css?family=Roboto:400,300,500' rel='stylesheet' type='text/css'/>
                {scripts.map(script => <script src={script}></script>)}
              </body>
            </html>
          )
        }

      case req@ContextPath(_, Seg("slick-crud-api" :: (name@EditorList(ed)) :: s)) ⇒
        authorizeUser(req) {
          handleRoute(Body.string(req), s, Autowire.route[Editor](ed), name)
        }

      case req@ContextPath(_, Seg("slick-crud-api" :: (name@"editors") :: s)) ⇒
        authorizeUser(req) {
          handleRoute(Body.string(req), s, Autowire.route[Editors](EditorList), name)
        }

      case req@ContextPath(_, Seg(script :: Nil)) if scripts contains script ⇒
        authorizeUser(req) {
          Option(getClass.getResourceAsStream(s"/$script"))
            .fold[ResponseFunction[Any]](NotFound)(ResponseStream)
        }
    }
  }
}
