package helloworld.algebra

import org.http4s.HttpRoutes

trait AbstractService[F[_]] {
  def getInstance(): HttpRoutes[F]
}
