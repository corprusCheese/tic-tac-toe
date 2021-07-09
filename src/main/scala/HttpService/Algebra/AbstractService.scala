package helloworld
package HttpService.Algebra

import cats.effect.IO
import org.http4s.HttpRoutes

trait AbstractService {
  def getInstance() : HttpRoutes[IO]
}
