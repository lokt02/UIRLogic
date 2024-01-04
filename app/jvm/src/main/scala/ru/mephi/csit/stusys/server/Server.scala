package ru.mephi.csit.stusys.server

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import twirl.Implicits._
import ru.mephi.csit.stusys.proplogic._

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.io.StdIn

object Server {

  def main(args: Array[String]): Unit = {

    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "my-system")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext: ExecutionContextExecutor = system.executionContext

    val route = concat(
      post{
        cors() {
          path("grade") {
            entity(as[String]) { logicalExpression =>
              val result: Future[String] = Future {
                parseExpression(logicalExpression) match {
                  case Right(value) => "SUCCESS: " + showExpression(value)
                  case Left(_) => "FAILURE!"
                }
              }
              onSuccess(result) { res =>
                complete(res)
              }
            }
          }
        }
      },
      get {
        cors() {
          path("task") {
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, showExpression(generateRandomLogicalExpression(2))))
          }
        }
      },
      pathSingleSlash {
        get {
          complete {
            templates.html.index.render()
          }
        }
      } ~
        pathPrefix("assets" / Remaining) { file =>
          // optionally compresses the response with Gzip or Deflate
          // if the client accepts compressed responses
          encodeResponse {
            getFromResource("public/" + file)
          }
        }
    )

    val bindingFuture = Http().newServerAt("localhost", 8080).bind(route)

    println(s"Server now online. Please navigate to http://localhost:8080\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}