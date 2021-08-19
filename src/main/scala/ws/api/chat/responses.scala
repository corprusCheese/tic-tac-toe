package ws.api.chat

import io.circe.syntax.EncoderOps
import ws.api.chat.messages._
import ws.api.model.json._
import ws.api.model._

object responses {

  def responseToJsonString(response: ChatResponse): String =
    response.asJson.toString()

  def createResponse(message: String): ChatResponse =
    matchSplitArray(message.split(" "))

  private def matchSplitArray(array: Array[String]): ChatResponse =
    array.toList match {

      /** target message implementation */
      case sender :: mark :: _ if mark == targetMarkMessage =>
        PrivateMessage(sender.trim, array.slice(2, array.length).mkString(" "))

      /** message to me */
      case usersMark :: _ if usersMark == usersMarkMessage =>
        MessageToMe(array.slice(1, array.length).mkString(" "))

      /** usual message */
      case sender :: _ =>
        array.mkString(" ") match {
          case value if value == notifyMessageOfWrongUser || value == notifyMessageOfSending =>
            MessageToMe(value)
          case _ =>
            PublicMessage(sender.dropRight(1).trim, array.slice(1, array.length).mkString(" "))
        }

      /** error */
      case _ =>
        ErrorResponse("Wrong formatted message")
    }
}
