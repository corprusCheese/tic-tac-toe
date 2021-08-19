package ws.api

package object model {

  sealed trait ChatRequest
  final case class SetName(name: String)                             extends ChatRequest
  final case class PublishMessage(message: String)                   extends ChatRequest
  final case class SendPrivateMessage(name: String, message: String) extends ChatRequest

  sealed trait ChatResponse
  final case class PublicMessage(from: String, message: String)  extends ChatResponse
  final case class PrivateMessage(from: String, message: String) extends ChatResponse
  final case class ErrorResponse(description: String)            extends ChatResponse
  final case class MessageToMe(message: String)                  extends ChatResponse

}
