package ws.api.model

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

object json {

  implicit val decodeSetName: Decoder[SetName] = (c: HCursor) => {
    for {
      req     <- c.downField("request").as[String]
      name    <- c.downField("payload").downField("name").as[String]
      setName = if (req == "set_name") name else ""
    } yield SetName(setName)
  }

  implicit val decodeChatRequest: Decoder[ChatRequest] = (c: HCursor) => {
    for {
      request <- c.downField("request").as[String]
      message <- c.downField("payload").downField("message").as[String]

      res = request match {
        case "publish_message" =>
          PublishMessage(message)
        case "send_private_message" =>
          val either = for {
            name <- c.downField("payload").downField("name").as[String]
          } yield name

          either match {
            case Left(_)     => PublishMessage("")
            case Right(name) => SendPrivateMessage(name, message)
          }
        case _ =>
          PublishMessage("")
      }
    } yield res
  }

  implicit val decodePublishMessage: Decoder[PublishMessage] = (c: HCursor) => {
    for {
      request <- c.downField("request").as[String]
      message <- c.downField("payload").downField("message").as[String]
      res = request match {
        case "publish_message" =>
          PublishMessage(message)
      }
    } yield res
  }

  implicit val decodeSendPrivateMessage: Decoder[SendPrivateMessage] = (c: HCursor) => {
    for {
      request <- c.downField("request").as[String]
      name    <- c.downField("payload").downField("name").as[String]
      message <- c.downField("payload").downField("message").as[String]
      res = request match {
        case "send_private_message" =>
          SendPrivateMessage(name, message)
      }
    } yield res
  }

  implicit val encodeChatResponse: Encoder[ChatResponse] = {
    case m: PublicMessage  => encodePublicMessage(m)
    case m: PrivateMessage => encodePrivateMessage(m)
    case m: ErrorResponse  => encodeErrorResponse(m)
    case m: MessageToMe    => encodeMessageToMe(m)
  }

  implicit val encodePublicMessage: Encoder[PublicMessage] = (a: PublicMessage) =>
    Json.obj(
      ("response", "public_message".asJson),
      ("data", (Map("message" -> a.message.asJson, "from" -> a.from.asJson)).asJson)
    )

  implicit val encodePrivateMessage: Encoder[PrivateMessage] = (a: PrivateMessage) =>
    Json.obj(
      ("response", "private_message".asJson),
      ("data", (Map("message" -> a.message.asJson, "from" -> a.from.asJson)).asJson)
    )

  implicit val encodeErrorResponse: Encoder[ErrorResponse] = (a: ErrorResponse) =>
    Json.obj(
      ("response", "error".asJson),
      ("data", (Map("message" -> a.description.asJson)).asJson)
    )

  implicit val encodeMessageToMe: Encoder[MessageToMe] = (a: MessageToMe) =>
    Json.obj(
      ("response", "system_message".asJson),
      ("data", (Map("message" -> a.message.asJson)).asJson)
    )
}
