import io.circe._
import io.circe.generic.semiauto._
import io.circe.disjunctionCodecs._

sealed trait Message
case class FormulaUpdate(x: Char, y: Int, formula: Option[String]) extends Message
case class CellUpdate(x: Char, y: Int, value: Either[String,Option[BigInt]]) extends Message




///////////////////////////
// JSON De/serialization //
///////////////////////////

object Message {
  implicit val encodeMessage: Encoder[Message] =
    deriveEncoder[Message]

  implicit val decodeMessage: Decoder[Message] =
    deriveDecoder[Message]
}