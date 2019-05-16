import akka.actor._

case class Start(client: ActorRef)

class SpreadSheet extends Actor with ActorLogging {  
  def receive = {
    case Start(client) =>
      context.become(connected(client))
  }

  def connected(client: ActorRef): Receive = {    
    case FormulaUpdate(x,y,None) =>
      log.info(s"Cell $x$y cleared")
      client ! CellUpdate(x,y,Right(None))
    case FormulaUpdate(x,y,Some(value)) =>
      log.info(s"Cell $x$y = '$value'")
      client ! CellUpdate(x,y,Left("N/A"))
  }
}