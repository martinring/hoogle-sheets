import akka.actor._
import akka.http.scaladsl._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws.{ Message => WSMessage, TextMessage }
import akka.http.scaladsl.server.Directives._
import akka.stream._
import akka.util.Timeout
import akka.pattern.ask
import akka.stream.scaladsl._
import scala.io.StdIn
import scala.concurrent.duration._
import scala.concurrent.Future
import io.circe.parser._
import io.circe.syntax._

object Main extends App {
  implicit val system = ActorSystem("akka-system")
  implicit val materializer = ActorMaterializer()
  implicit val dispatcher = system.dispatcher
  implicit val timeout = Timeout(5 seconds) 

  def socket: Flow[WSMessage,WSMessage,Any] = {
    val spreadSheet = system.actorOf(Props[SpreadSheet])
    val spreadSheetSink = Sink.actorRef[Message](spreadSheet,PoisonPill)

    val in = Flow[WSMessage].collect {
        case TextMessage.Strict("pong") => None
        case TextMessage.Strict(text) => decode[Message](text).toOption
      }
      .collect { case Some(msg) => msg }
      .to(spreadSheetSink)
    
    val out = Source.actorRef[Message](1, OverflowStrategy.fail)
      .mapMaterializedValue(spreadSheet ! Start(_))
        
    Flow.fromSinkAndSource(in,out)      
      .map(msg => TextMessage.Strict(msg.asJson.noSpaces))    
      .keepAlive(20 seconds, () => TextMessage.Strict("ping"))
      .idleTimeout(30 seconds)
  }

  val route = 
    extractUpgradeToWebSocket { _ =>    
      handleWebSocketMessages(socket)      
    } ~    
    getFromResourceDirectory(".") ~
    getFromResource("index.html")

  val binding = Http().bindAndHandle(route,"localhost",3000)

  StdIn.readLine()
  binding.flatMap(_.unbind()).onComplete(_ => system.terminate())
}