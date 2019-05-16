import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import io.circe.parser._
import io.circe.syntax._
import scala.util.Try

/**
 * Die JavaScript Client Anwendung
 */
object Main extends JSApp {
  def main(): Unit = {
    // Wir bauen eine Websocket Verbindung zum Server Auf
    val socket = new WebSocket("ws://" + window.location.host)
    socket.onopen = (d) => {
      renderSheet(upd => socket.send(upd.asJson.noSpaces))
    }

    // Hier reagieren wir auf Nachrichten die wir vom Server bekommen
    socket.onmessage = (e) => {      
      e.data.toString match {
        case "ping" => socket.send("pong")
        case msg => decode[Message](e.data.toString).fold(throw _, {
          // Wenn ein Fehler beim dekodieren aufgetreten ist, werfe ihn auf der Konsole
          // Sonst verarbeite die Nachricht          
            case CellUpdate(x,y,Right(value)) =>
              renderCell(x,y,value.map(_.toString).getOrElse(""))
            case CellUpdate(x,y,Left(error)) =>
              renderCell(x,y,error,true)
            case other =>
              console.error("unhandled message", other.toString)
          })
      }      
    }

    // Wenn die Verbindung geschlossen wird, zeigen wir eine Fehlermeldung an.
    socket.onclose = (e) => {
      document.getElementById("sheet").innerHTML =
        "<span class='error'>Die Verbindung zum Server wurde geschlossen</span>"
    }
  }

  def renderCell(x: Char, y: Int, content: String, error: Boolean = false) = {
    Try(document.querySelector(s"#cell-$x-$y").asInstanceOf[html.TableDataCell]).foreach { elem => 
      elem.classList.remove("loading")
      if (error) elem.classList.add("error") else elem.classList.remove("error")
      Try(elem.firstChild.asInstanceOf[html.Span]).foreach(_.innerHTML = content)
    }    
  }  

  def initCell(x: Char, y: Int, send: Message => Unit): html.TableDataCell = {
    val cell = document.createElement("td").asInstanceOf[html.TableDataCell]
    cell.id = s"cell-$x-$y"
    val content = document.createElement("span").asInstanceOf[html.Span]
    content.classList.add("content")
    cell.appendChild(content)
    var formula = Option.empty[String]
    cell.onclick = (e) => {
      val input = document.createElement("input").asInstanceOf[html.Input]
      input.`type` = "text"      
      input.value = formula.getOrElse("")
      cell.classList.add("editing")
      cell.appendChild(input)      
      input.focus()      
      input.oninput = (e) => {
        val nf = input.value.trim match {
          case "" => None
          case x => Some(x)
        }
        if (nf != formula) send(FormulaUpdate(x,y,nf))
        formula = nf
      }      
      input.onblur = (e) => {
        cell.removeChild(input)
        cell.classList.remove("editing")
      }
    }
    cell
  }

  def renderSheet(send: Message => Unit) = {
    val sheet = document.getElementById("sheet").asInstanceOf[html.Div]
    val table = document.createElement("table").asInstanceOf[html.Table]    
    for (y <- 0 to 100) {
      val row = document.createElement("tr").asInstanceOf[html.TableRow]
      for (x <- '@' to 'Z') {
        if (x == '@' || y == 0) {
          val header = document.createElement("th").asInstanceOf[html.TableHeaderCell]          
          val id = if (x == '@') y.toString else x.toString        
          header.id = s"head-$id"
          header.innerHTML = id
          row.appendChild(header)
        } else {
          row.appendChild(initCell(x,y,send))
        }
      }
      table.appendChild(row)
    }
    sheet.appendChild(table)
  }
}