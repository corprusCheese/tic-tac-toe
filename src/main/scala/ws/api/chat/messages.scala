package ws.api.chat

import cats.Monad

object messages {

  lazy val targetMarkMessage: String        = "(You):"
  lazy val usersMarkMessage: String         = "users:"
  lazy val notifyMessageOfSending: String   = "You send the message!"
  lazy val notifyMessageOfWrongUser: String = "There is no such user!"

  def emptyAction[F[_]: Monad](): F[Unit] = Monad[F].pure(())
}
